{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Brick
import qualified Brick
import qualified Brick.Main             as Brick
import           Brick.Util             (on)
import qualified Brick.Widgets.Border   as B
import qualified Brick.Widgets.Center   as C
import qualified Brick.Widgets.Dialog   as Dialog
import           Brick.Widgets.Edit     (Editor)
import qualified Brick.Widgets.Edit     as Editor
import qualified Client
import           Control.Monad          (join, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Graphics.Vty           as V
import           Lens.Micro.Platform
import           Network.URI            (URI)
import qualified Network.URI            as URI
import qualified Output

data Loadable a = NotStarted | Loading | Loaded a deriving (Show)

data State = State
  { _currentURI    :: Maybe URI
  , _geminiContent :: Loadable Client.GeminiResponse
  , _urlEditor     :: Editor String Name
  , _logs          :: [String]
  } deriving (Show)

data Name = UrlEdit | ContentVP deriving (Show, Eq, Ord)
type Event = ()

app :: App State Event Name
app = App { appDraw = drawUI
          , appChooseCursor = \_ -> showCursorNamed UrlEdit
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const myAttrMap
          }

myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr []

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey (V.KEnter) [])) = liftIO (doFetch s) >>= continue
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent ev) =
  Editor.handleEditorEvent ev (_urlEditor s) >>= (\it -> continue $ s {_urlEditor = it})

doFetch :: State -> IO State
doFetch s = do
  -- TODO: do this asynchronously
  let editContents = head $ Editor.getEditContents $ _urlEditor s
  case URI.parseURI editContents of
    Just uri ->
      responseToState s <$> Client.get uri
    Nothing ->
      return s

responseToState :: State -> Maybe Client.GeminiResponse -> State
responseToState s (Just geminiResponse@(Client.GeminiResponse uri _)) =
  s { _geminiContent = Loaded geminiResponse, _currentURI = Just uri}
responseToState s Nothing = s

drawUI :: State -> [Widget Name]
drawUI s =
  [
    C.center $ B.border $
      vBox [urlEditor, B.hBorder, outputVp, B.hBorder, drawStatusLine s]
  ]
  where urlEditor = (Editor.renderEditor (str . unlines) True (_urlEditor s))
        outputVp = viewport ContentVP Vertical $ str (debugGeminiContent s)

drawStatusLine :: State -> Widget Name
drawStatusLine s = padRight Max status <+> help
  where status = case _currentURI s of
          Just uri -> str ("Currently at " <> show uri)
          Nothing  -> str "Nowhere ;("
        help = str "(Ctrl+c to quit)"
debugGeminiContent :: State -> String
debugGeminiContent s = _geminiContent s & show


initState :: State
initState = State
  { _currentURI = Nothing
  , _geminiContent = NotStarted
  , _urlEditor = Editor.editor UrlEdit (Just 1) "gemini://gemini.circumlunar.space/"
  , _logs = []
  }

main :: IO ()
main = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initState
