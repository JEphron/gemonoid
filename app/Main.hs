{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Client

{-
TODO:
* ability to have a persistent session
* navigatable links
* fix certificate handling: don't use system trust anchors
* handle response codes properly
-}

-- data ResponseHeader = ResponseHeader {status :: Status, meta :: String}
-- data Mime = TextPlain | CustomMime String
-- data ResponseBody = ResponseBody Text
-- data Response = Response ResponseHeader (Maybe ResponseBody)

main = Client.get "gemini://envs.net"
