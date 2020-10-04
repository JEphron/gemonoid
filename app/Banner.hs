{-# LANGUAGE QuasiQuotes #-}

module Banner (draw, attrs) where

import Brick (AttrMap, AttrName, Widget, fg, hBox, str, vBox, withAttr)
import Data.String (fromString)
import Graphics.Vty
  ( Attr,
    Color,
    blue,
    cyan,
    green,
    magenta,
    red,
    yellow,
  )
import Raw
import Utils (imap)

banner :: String
banner =
  [rw|
  ____  ___ __  __  ___  _  _  ___ ___ ____
 / ___|| __|  \/  |/ _ \| \| |/ _ \_ _|  _ \
| |  _ | _|| |\/| | (_) | .` | (_) | || | | |
| |_| ||___|_|  |_|\___/|_|\_|\___/___| |_| |
 \____|                               |____/ |]

attrName :: Int -> String
attrName i =
  "bannerRainbow" <> show i

rainbow :: [Color]
rainbow =
  [red, green, blue, yellow, magenta, cyan]

attrs :: [(AttrName, Attr)]
attrs =
  let toAttr i color =
        (fromString (attrName i), fg color)
   in imap toAttr rainbow

makeAttrName :: Int -> AttrName
makeAttrName i =
  fromString $ attrName $ i `mod` (length attrs)

drawLine :: Int -> String -> Widget a
drawLine i string =
  hBox $
    imap
      ( \j char ->
          withAttr (makeAttrName (i + j)) $
            str [char]
      )
      string

draw :: Widget a
draw =
  vBox $ imap drawLine (lines banner)
