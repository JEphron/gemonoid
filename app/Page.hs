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

data Page = Page
  { _uri :: URI,
    _content :: Loadable GeminiResponse
  }
  deriving (Show)

makeLenses ''Page

blank :: Page
blank =
  Page BlankPage.uri (Loaded BlankPage.content)

links :: Page -> [URI]
links page =
  case page ^. content of
    Loaded (Success (GeminiResource lines)) ->
      [uri | LinkLine (Right uri) _ <- lines]
    _ ->
      []

isLoaded :: Page -> Bool
isLoaded page =
  page ^. content & Loadable.isResolved
