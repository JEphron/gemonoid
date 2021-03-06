{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Control.Applicative (liftA2)
import qualified Control.Exception as E
import Control.Monad (join)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Char as Char
import Data.Default.Class (def)
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Lens.Micro.Platform
import Network.Socket
import qualified Network.TLS as TLS
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Network.TLS.SessionManager as TLSSessionManager
import Network.URI
import Safe
import System.Timeout
import System.X509 (getSystemCertificateStore)

type MimeType = Text

data HeadingLevel
  = H1
  | H2
  | H3
  deriving (Show, Eq)

data Line
  = TextLine Text
  | LinkLine (Either Text URI) Text -- url, description
  | PreLine Text
  | HeadingLine HeadingLevel Text
  | ULLine Text
  | QuoteLine Text
  deriving (Show)

newtype GeminiPage = GeminiPage {pageLines :: [Line]}

instance Show GeminiPage where
  show GeminiPage {pageLines} =
    "\n" ++ List.intercalate "\n" (map show pageLines)

data Content
  = GeminiContent GeminiPage
  | UnknownContent MimeType Text
  deriving (Show)

data FailureInfo = FailureInfo
  { failureReason :: String,
    permanent :: Bool
  }
  deriving (Show)

data GeminiInnerResponse
  = Input Text -- prompt
  | Success Content
  | Redirect URI
  | Failure FailureInfo
  | CertRequest
  | Unknown (Int, Int) Text
  deriving (Show)

data GeminiResponse
  = GeminiResponse URI GeminiInnerResponse
  deriving (Show)

newtype GeminiRequest = GeminiRequest URI

maxRedirects :: Int
maxRedirects = 4

get :: URI -> IO (Maybe GeminiResponse)
get initialUri = innerGet initialUri maxRedirects
  where
    innerGet uri remainingRedirects =
      let fail msg =
            return $
              Just $
                GeminiResponse uri $
                  Failure $
                    FailureInfo {failureReason = msg, permanent = False}

          handleResponse response =
            case parseResponse uri response of
              Just (GeminiResponse _ (Redirect newUri)) -> do
                innerGet newUri (remainingRedirects - 1)
              other ->
                return other
       in if uriScheme uri /= "gemini:"
            then fail "I only understand Gemini URLs! Get a real browser!"
            else
              if remainingRedirects == 0
                then fail "Exceeded max retries!"
                else do
                  responseOrException <- tryAny (getRaw uri)
                  case responseOrException of
                    Left exc ->
                      fail $ "Unknown failure: " <> show exc
                    Right (Left exc) ->
                      fail $ "Unknown failure: " <> show exc
                    Right (Right response) ->
                      handleResponse response

parseResponse :: URI -> Text -> Maybe GeminiResponse
parseResponse uri text = do
  let lines = T.lines text
  let mkResp d = Just $ GeminiResponse uri d
  (header, body) <- List.uncons lines
  (status, meta) <- parseMeta header
  case status of
    (1, _) -> mkResp $ Input meta
    (2, _) -> do
      mime <- parseMimeType meta
      mkResp $ Success $ parseSuccess uri mime body
    (3, _) -> do
      uri <- parseURI (T.unpack meta)
      mkResp $ Redirect uri
    other -> mkResp $ Unknown other meta

getRaw :: URI -> IO (Either UnicodeException Text)
getRaw uri =
  let host = fromJust $ uriRegName <$> uriAuthority uri
   in runTCPClient host "1965" $ \sock -> -- todo: handle alternative port numbers
        withTLS host sock $ \ctx -> do
          let toSend = LC.pack $ uriToString id uri "\r\n"
          TLS.sendData ctx toSend
          response <- recvAll ctx
          return (decodeUtf8' response)

parseSuccess :: URI -> MimeType -> [Text] -> Content
parseSuccess uri "text/gemini" lines = GeminiContent $ parseGeminiPage uri lines
parseSuccess _uri mime lines = UnknownContent mime (T.unlines lines)

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

parseMimeType :: Text -> Maybe MimeType
parseMimeType = headMay . T.splitOn ";" -- ignoring params for now

parseGeminiPage :: URI -> [Text] -> GeminiPage
parseGeminiPage uri = GeminiPage . parseLines uri

parseURIT :: Text -> Maybe URI
parseURIT = parseURI . T.unpack

isRelativeReferenceT :: Text -> Bool
isRelativeReferenceT = isRelativeReference . T.unpack

isAbsoluteURIT :: Text -> Bool
isAbsoluteURIT = isAbsoluteURI . T.unpack

parseRelativeReferenceT :: Text -> Maybe URI
parseRelativeReferenceT = parseRelativeReference . T.unpack

calcUri :: Text -> URI -> Maybe URI
calcUri incomingUriStr currentUri
  | isRelativeReferenceT incomingUriStr =
    liftA2
      relativeTo
      (parseRelativeReferenceT incomingUriStr)
      (pure currentUri)
  | isAbsoluteURIT incomingUriStr = parseURIT incomingUriStr
  | otherwise = Nothing

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing e = Left e

parseLine :: URI -> Bool -> Text -> Line
parseLine currentUri isPre line
  | isPre = PreLine line
  | line `startsWith` ">" = QuoteLine (munge 1 line)
  | line `startsWith` "=>" =
    let (uriStr, description) = munge 2 line & T.break Char.isSpace
        uri = calcUri uriStr currentUri
     in LinkLine (maybeToEither uri uriStr) (T.strip description)
  | line `startsWith` "*" = ULLine (munge 1 line)
  | line `startsWith` "###" = HeadingLine H3 (munge 3 line)
  | line `startsWith` "##" = HeadingLine H2 (munge 2 line)
  | line `startsWith` "#" = HeadingLine H1 (munge 1 line)
  | otherwise = TextLine line
  where
    munge n = dropWhitespace . T.drop n
    dropWhitespace = T.dropWhile Char.isSpace

parseLines :: URI -> [Text] -> [Line]
parseLines uri =
  fst
    . foldl
      ( \(lines, isPre) line ->
          let nextIsPre = (line `startsWith` "```") `xor` isPre
              aboutToChange = (nextIsPre && not isPre) || (isPre && not nextIsPre)
           in if aboutToChange
                then (lines, nextIsPre) -- skip ```
                else (lines ++ [parseLine uri (isPre && nextIsPre) line], nextIsPre)
      )
      ([], False)
  where
    xor = (/=)

recvAll :: TLS.Context -> IO C.ByteString
recvAll ctx =
  let timeoutMs = 8000
      recvAll' str = do
        resp <- tryAny (timeout (1000 * timeoutMs) $ TLS.recvData ctx)
        case resp of
          Right (Just "") -> return str
          Right (Just newPart) -> recvAll' (str <> newPart)
          Right Nothing -> do
            return str
          Left _ -> return str
   in recvAll' ""

rightToJust :: Either l r -> Maybe r
rightToJust (Right r) = Just r
rightToJust (Left _) = Nothing

tryAny :: IO a -> IO (Either E.SomeException a)
tryAny = E.try

safeGetAddrInfo :: Maybe AddrInfo -> Maybe HostName -> Maybe ServiceName -> IO (Maybe [AddrInfo])
safeGetAddrInfo addrInfo hostName serviceName =
  join . rightToJust
    <$> ( tryAny
            . timeout oneSecond
            $ getAddrInfo addrInfo hostName serviceName
        )
  where
    oneSecond = 1000 * 1000

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient url port fn =
  let resolve = do
        let hints = defaultHints {addrSocketType = Stream}
        fmap headMay <$> safeGetAddrInfo (Just hints) (Just url) (Just port)

      open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock
   in do
        addrMay <- resolve
        case join addrMay of
          Just addr ->
            E.bracket (open addr) close fn
          Nothing ->
            error "unable to resolve addr"

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
                { TLS.onServerCertificate = \_store _cache _serviceId _certChain -> return [],
                  TLS.onCertificateRequest = \_ -> do
                    return Nothing
                }
          }
   in do
        sessionManager <- TLSSessionManager.newSessionManager TLSSessionManager.defaultConfig
        store <- getSystemCertificateStore
        ctx <- TLS.contextNew (TLS.getBackend socket) (tlsParams store sessionManager)
        -- TLS.contextHookSetLogging ctx logging
        E.bracket_ (TLS.handshake ctx) (TLS.bye ctx) (fn ctx)
