{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Brick                  (App (..), AttrMap, BrickEvent (..),
                                         EventM, Next, Widget, (<+>), (<=>))
import qualified Brick
import qualified Brick.Main             as Brick
import           Brick.Util             (on)
import qualified Brick.Widgets.Center   as Center
import qualified Brick.Widgets.Dialog   as Dialog
import           Brick.Widgets.Edit     (Editor)
import qualified Brick.Widgets.Edit     as Editor
import qualified Client
import           Control.Monad          (join, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Graphics.Vty           as Vty
import           Lens.Micro.Platform
import qualified Network.URI            as URI
import qualified Output

{-
TODO:
* ability to have a persistent session
* navigatable links
* fix certificate handling: don't use system trust anchors
* handle response codes properly
-}

data Loadable e a = NotStarted | Loading | Loaded a | Failed e deriving (Show)

data Name = URLEditor deriving (Eq, Ord, Show)

type Event = ()

data UrlBarState = UrlBarState
  { _active      :: Bool,
    _editorState :: Editor String Name
  }
  deriving (Show)

type Session = ()

data State = State
  { _session     :: Session,
    _urlBarState :: UrlBarState,
    _geminiPage  :: Loadable String Client.GeminiResponse
  }
  deriving (Show)

makeLenses ''State
makeLenses ''UrlBarState

initState :: State
initState =
  State
    { _session = (),
      _urlBarState = initEnterUrl,
      _geminiPage = NotStarted
    }

initEnterUrl :: UrlBarState
initEnterUrl =
  UrlBarState
    { _active = False,
      _editorState = Editor.editor URLEditor (Just 1) ""
    }

app :: App State Event Name
app =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvent,
      appChooseCursor = Brick.showFirstCursor,
      appStartEvent = return,
      appAttrMap = const attrMap
    }

attrMap :: AttrMap
attrMap =
  Brick.attrMap
    Vty.defAttr
    [ (Editor.editAttr, Vty.white `on` Vty.blue),
      (Editor.editFocusedAttr, Vty.black `on` Vty.yellow)
    ]

drawUI :: State -> [Widget Name]
drawUI state =
  let urlInput = Editor.renderEditor (Brick.str . unlines) True (state ^. urlBarState . editorState)
   in [ Brick.str "Pollux"
          <=> (Brick.str "enter url: " <+> urlInput)
          -- <=> maybe (Brick.str "No error yet...") Brick.str (subState ^. ssError)
      ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent state ev@(VtyEvent event) =
  withQuitHandler state event $
    ( case event of
        Vty.EvKey someKey someMod ->
          if state ^. urlBarState . active
            then handleUrlbar state event
            else handlePageEvents state event
    )

handlePageEvents :: State -> Vty.Event -> EventM Name (Next State)
handlePageEvents = undefined

handleUrlbar :: State -> Vty.Event -> EventM Name (Next State)
handleUrlbar state event =
  case event of
    Vty.EvKey Vty.KEnter [] ->
      Brick.continue $ set (urlBarState . active) False state
    Vty.EvKey someKey someMod -> do
      newEditorState <- Editor.handleEditorEvent event (view (urlBarState . editorState) state)
      Brick.continue $ set (urlBarState . editorState) newEditorState state

withQuitHandler :: State -> Vty.Event -> EventM Name (Next State) -> EventM Name (Next State)
withQuitHandler state (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) _ = Brick.halt state
withQuitHandler _ _ f = f

-- doFetchGeminiStuff = do
--   let editContents = T.unpack $ T.strip $ T.unlines $ Editor.getEditContents $ subState ^. ssUrlEdit
--   newSubState <- case URI.parseURI editContents of
--     Just uri -> do
--       geminiResponseMay <- liftIO $ Client.get uri
--       case geminiResponseMay of
--         Just geminiResponse ->
--           return $ subState & ssError .~ Nothing
--         Nothing ->
--           return $ subState & ssError ?~ "bad response"
--     Nothing ->
--       return $ subState & ssError ?~ ("bad uri: '" <> editContents <> "'")

--Brick.continue (state & stSubState ssUrlEdit)

main :: IO ()
main = void $ Brick.defaultMain app initState
