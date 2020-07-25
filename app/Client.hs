{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Char as Char
import Data.Default.Class (def)
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro.Platform
import Lib
import qualified Log
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.TLS as TLS
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Network.TLS.SessionManager as TLSSessionManager
import Network.URI
import Safe
import Status
import System.Timeout
import System.X509 (getSystemCertificateStore)

type MimeType = Text

data HeadingLevel
  = H1
  | H2
  | H3
  deriving (Show)

data Line
  = TextLine Text
  | LinkLine Text Text
  | PreLine Text
  | HeadingLine HeadingLevel Text
  | ULLine Text
  | QuoteLine Text
  deriving (Show)

newtype GeminiPage = GeminiPage {_lines :: [Line]}

instance Show GeminiPage where
  show GeminiPage {_lines} =
    "\n" ++ List.intercalate "\n" (map show _lines)

data Content
  = GeminiContent GeminiPage
  | UnknownContent MimeType Text
  deriving (Show)

data FailureInfo = FailureInfo
  { failureReason :: String,
    permanent :: Bool
  }
  deriving (Show)

data GeminiResponse
  = Input Text -- prompt
  | Success Content
  | Redirect URI
  | Failure FailureInfo
  | CertRequest
  | Unknown (Int, Int) Text
  deriving (Show)

newtype GeminiRequest = GeminiRequest URI

get :: String -> IO (Maybe GeminiResponse)
get uriString = do
  response <- parseResponse <$> getRaw (fromJust $ parseURI uriString)
  putStrLn "---------------- PARSED RESPONSE ---------------"
  return response

parseMeta :: Text -> Maybe ((Int, Int), Text)
parseMeta =
  let parseMeta' (s1 : s2 : rest) =
        let status = (read [s1], read [s2])
            meta = takeWhile (/= '\r') (dropWhile Char.isSpace rest)
         in Just (status, T.pack meta)
      parseMeta' _ = Nothing
   in parseMeta' . T.unpack -- stupid String

startsWith :: Text -> Text -> Bool
startsWith txt beg = T.take (T.length beg) txt == beg

xor a b = a /= b

dropWhitespace = T.dropWhile Char.isSpace

parseLine :: Bool -> Text -> Line
parseLine isPre line
  | isPre = PreLine line
  | line `startsWith` ">" = QuoteLine (munge 1 line)
  | line `startsWith` "=>" =
    uncurry LinkLine $ dropWhitespace <$> T.breakOn " " (munge 2 line)
  | line `startsWith` "*" = ULLine (munge 1 line)
  | line `startsWith` "###" = HeadingLine H3 (munge 3 line)
  | line `startsWith` "##" = HeadingLine H2 (munge 2 line)
  | line `startsWith` "#" = HeadingLine H1 (munge 1 line)
  | otherwise = TextLine line
  where
    munge n = dropWhitespace . T.drop n

parseLines :: [Text] -> [Line]
parseLines =
  fst
    . foldl
      ( \(lines, isPre) line ->
          let nextIsPre = (line `startsWith` "```") `xor` isPre
           in (lines ++ [parseLine (isPre && nextIsPre) line], nextIsPre)
      )
      ([], False)

parseGeminiPage :: [Text] -> GeminiPage
parseGeminiPage = GeminiPage . parseLines

parseSuccess :: MimeType -> [Text] -> Content
parseSuccess "text/gemini" lines = GeminiContent $ parseGeminiPage lines
parseSuccess mime lines = UnknownContent mime (T.unlines lines)

parseMimeType :: Text -> Maybe MimeType
parseMimeType = headMay . T.splitOn ";" -- ignoring params for now

parseResponse :: Text -> Maybe GeminiResponse
parseResponse text = do
  let lines = T.lines text
  (header, body) <- List.uncons lines
  (status, meta) <- parseMeta header
  case status of
    (1, _) -> Just $ Input meta
    (2, _) -> do
      mime <- parseMimeType meta
      Just $ Success $ parseSuccess mime body
    (3, _) -> do
      uri <- parseURI (T.unpack meta) -- todo: handle relative uris
      Just $ Redirect uri
    other -> Just $ Unknown other meta

getRaw :: URI -> IO Text
getRaw uri =
  let host = fromJust $ uriRegName <$> uriAuthority uri
   in runTCPClient host "1965" $ \sock ->
        withTLS host sock $ \ctx -> do
          let toSend = LC.pack $ uriToString id uri "\r\n"
          Log.info ("Sending..." <> show toSend)
          TLS.sendData ctx toSend
          response <- recvAll ctx
          C.putStrLn response
          return (decodeUtf8 response)

recvAll :: TLS.Context -> IO C.ByteString
recvAll ctx =
  let timeoutMs = 8000

      recvData :: IO (Either E.SomeException (Maybe C.ByteString))
      recvData = E.try (timeout (1000 * timeoutMs) $ TLS.recvData ctx)

      recvAll' str = do
        Log.info "receving..."
        resp <- recvData
        case resp of
          Right (Just "") -> return str
          Right (Just newPart) -> recvAll' (str <> newPart)
          Right Nothing -> do
            Log.error "timed out!"
            return str
          Left _ -> return str
   in recvAll' ""

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient url port fn =
  let resolve = do
        let hints = defaultHints {addrSocketType = Stream}
        fmap headMay (getAddrInfo (Just hints) (Just url) (Just port))

      open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock
   in do
        addrMay <- resolve
        case addrMay of
          Just addr ->
            E.bracket (open addr) close fn
          Nothing -> error "unable to resolve addr"

withTLS :: HostName -> Socket -> (TLS.Context -> IO a) -> IO a
withTLS hostname socket fn =
  let logging = packetLogging def
      packetLogging logging =
        logging
          { TLS.loggingPacketSent = putStrLn . ("debug (sent): " ++),
            TLS.loggingPacketRecv = putStrLn . ("debug (recv): " ++)
          }
      tlsParams store sessionManager =
        (TLS.defaultParamsClient hostname C.empty)
          { TLS.clientSupported =
              def
                { TLS.supportedVersions = [TLS.TLS13, TLS.TLS12],
                  TLS.supportedCiphers = ciphersuite_default
                },
            TLS.clientShared =
              def
                { TLS.sharedCAStore = store,
                  TLS.sharedSessionManager = sessionManager
                },
            TLS.clientHooks =
              def
                { TLS.onServerCertificate = \store cache serviceId certChain -> return [],
                  TLS.onCertificateRequest = \thing -> do
                    Log.info "ignoring client cert request"
                    return Nothing
                }
          }
   in do
        sessionManager <- TLSSessionManager.newSessionManager TLSSessionManager.defaultConfig
        store <- getSystemCertificateStore
        ctx <- TLS.contextNew (TLS.getBackend socket) (tlsParams store sessionManager)
        -- TLS.contextHookSetLogging ctx logging
        E.bracket_ (TLS.handshake ctx) (TLS.bye ctx) (fn ctx)
