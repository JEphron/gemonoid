{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
  ( App (..),
    AttrMap,
    BrickEvent (..),
    EventM,
    Next,
    Widget,
    (<+>),
    (<=>),
  )
import qualified Brick
import qualified Brick.Main as Brick
import Brick.Util (on)
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Dialog as Dialog
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.Edit as Editor
import qualified Client
import Control.Monad (join, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform
import qualified Network.URI as URI

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

data EnterURLState = EnterURLState
  { _ssUrlEdit :: Editor Text Name,
    _ssError :: Maybe String
  }
  deriving (Show)

data SubState
  = Welcome
  | EnterURL EnterURLState
  deriving (Show)

data State = State
  { _stGeminiContent :: Loadable String Client.GeminiResponse,
    _stSubState :: SubState
  }
  deriving (Show)

makeLenses ''State
makeLenses ''EnterURLState

initState :: State
initState =
  State
    { _stGeminiContent = NotStarted,
      _stSubState = initEnterUrl
    }

initEnterUrl =
  EnterURL
    EnterURLState
      { _ssUrlEdit = Editor.editorText URLEditor (Just 1) "",
        _ssError = Nothing
      }

app :: App State Event Name
app =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvent,
      appChooseCursor = Brick.showFirstCursor,
      appStartEvent = return,
      appAttrMap = const theMap
    }

theMap :: AttrMap
theMap =
  Brick.attrMap
    Vty.defAttr
    [ (Editor.editAttr, Vty.white `on` Vty.blue),
      (Editor.editFocusedAttr, Vty.black `on` Vty.yellow)
    ]

drawUI :: State -> [Widget Name]
drawUI state =
  case state ^. stSubState of
    Welcome -> [Brick.str "Welcome!"]
    EnterURL subState ->
      let urlInput = Editor.renderEditor (Brick.txt . T.unlines) True (subState ^. ssUrlEdit)
       in [Brick.str "Pollux" <=> (Brick.str "enter url: " <+> urlInput) <=> maybe (Brick.str "No error yet...") Brick.str (subState ^. ssError)]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent state ev@(VtyEvent event) =
  case event of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt state
    Vty.EvKey Vty.KEsc [] -> Brick.halt state
    _ ->
      case state ^. stSubState of
        EnterURL subState ->
          case event of
            Vty.EvKey Vty.KEnter [] -> do
              let editContents = T.unpack $ T.strip $ T.unlines $ Editor.getEditContents $ subState ^. ssUrlEdit
              newSubState <- case URI.parseURI editContents of
                Just uri -> do
                  geminiResponseMay <- liftIO $ Client.get uri
                  case geminiResponseMay of
                    Just geminiResponse ->
                      return $ subState & ssError .~ Nothing
                    Nothing ->
                      return $ subState & ssError ?~ "bad response"
                Nothing ->
                  return $ subState & ssError ?~ ("bad uri: '" <> editContents <> "'")

              let newState = state & stSubState .~ EnterURL newSubState

              Brick.continue $ newState
            _ -> do
              newEditorState <- Editor.handleEditorEvent event (subState ^. ssUrlEdit)
              let newSubState = subState & ssUrlEdit .~ newEditorState
              Brick.continue $ state & stSubState .~ EnterURL newSubState

--Brick.continue (state & stSubState ssUrlEdit)

main :: IO ()
main = void $ Brick.defaultMain app initState
