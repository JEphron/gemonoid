module Utils where

import qualified Control.Exception as E
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.URI (URI)
import qualified Network.URI as URI

stripString :: String -> String
stripString =
  -- todo: jank
  T.unpack . T.strip . T.pack

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn xs =
  map (uncurry fn) $ zip [0 ..] xs

whenMaybe :: (Applicative f) => Bool -> f a -> f (Maybe a)
whenMaybe p s =
  if p then fmap Just s else pure Nothing

pop :: Vector a -> (Vector a, Maybe a)
pop v =
  if Vector.null v
    then (v, Nothing)
    else (Vector.init v, Just $ Vector.last v)

tryAny :: IO a -> IO (Either E.SomeException a)
tryAny =
  E.try

rightToJust :: Either l r -> Maybe r
rightToJust (Right r) = Just r
rightToJust (Left l) = Nothing

saneEscapeUri :: String -> String
saneEscapeUri =
  URI.escapeURIString URI.isAllowedInURI

setQuery :: URI -> String -> URI
setQuery uri query =
  uri {URI.uriQuery = "?" <> saneEscapeUri query}
