{-# LANGUAGE QuasiQuotes #-}

module BlankPage where

import Client
import Network.URI (URI)
import qualified Network.URI.Static as S

uri :: URI
uri =
  [S.uri|gemini://blank.localhost/|]

content :: GeminiResponse
content =
  Success (GeminiResource [TextLine "Hello World"])
