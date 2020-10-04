{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Page where

import qualified BlankPage
import Client
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lens.Micro.Platform
import Loadable (Loadable (..))
import qualified Loadable
import Network.URI (URI)
import qualified Network.URI as URI
import Prelude hiding (lines)

data Page = Page
  { _uri :: URI,
    _content :: Loadable GeminiResponse
  }
  deriving (Show)

makeLenses ''Page

blank :: Page
blank =
  Page BlankPage.uri (Loaded BlankPage.content)

loaded :: URI -> GeminiResponse -> Page
loaded uri response =
  Page uri (Loaded response)

loading :: URI -> Page
loading uri =
  Page uri Loading

links :: Page -> [URI]
links page =
  [uri | LinkLine (Right uri) _ <- lines page]

lines :: Page -> [Line]
lines page =
  case contentMay page of
    Just (Success (GeminiResource lines)) ->
      lines
    _ ->
      []

contentMay :: Page -> Maybe GeminiResponse
contentMay page =
  case page ^. content of
    Loaded c -> Just c
    _ -> Nothing

isLoaded :: Page -> Bool
isLoaded page =
  page ^. content & Loadable.isResolved

isLoading :: Page -> Bool
isLoading page =
  page ^. content & Loadable.isLoading
