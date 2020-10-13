{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Banner
import Brick
import qualified Brick
import qualified Brick.BChan as BChan
import qualified Brick.Focus as Focus
import qualified Brick.Main as Brick
import Brick.Util (on)
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.Dialog as Dialog
import Brick.Widgets.Edit (DecodeUtf8, Editor)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as BrickList
import Browser (Browser)
import qualified Browser
import Cache (Cache)
import qualified Cache
import Client
import Control.Applicative (liftA2)
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import Control.Monad (foldM, foldM_, forever, guard, join, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (mapReaderT)
import Control.Monad.State.Lazy (mapStateT)
import qualified Control.Monad.Trans.State.Lazy
import qualified Data.Char as Char
import Data.CircularList (CList)
import qualified Data.CircularList as CList
import Data.Functor.Identity
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graphics.Vty
  ( Attr,
    black,
    blue,
    bold,
    cyan,
    green,
    magenta,
    red,
    standout,
    underline,
    white,
    withStyle,
    withURL,
    yellow,
  )
import qualified Graphics.Vty as V
import History (History)
import qualified History
import Lens.Micro.Platform
import Loadable (Loadable (..))
import qualified Loadable
import Network.URI (URI)
import qualified Network.URI as URI
import Page (Page)
import qualified Page
import RequestProcessor
import System.Timeout
import Tab (Tab)
import qualified Tab
import Utils

mapWidget :: (a -> b) -> Widget a -> Widget b
mapWidget fn w@(Widget {render}) =
  w {render = (aaa fn render)}

aaa :: (a -> b) -> RenderM a (Result a) -> RenderM b (Result b)
aaa fn render = mapReaderT (mapStateT _) render

-- bbb ::
--   Control.Monad.Trans.State.Lazy.StateT
--     (RenderState a)
--     Data.Functor.Identity.Identity
--     (Result a) ->
--   Control.Monad.Trans.State.Lazy.StateT
--     (RenderState b)
--     Data.Functor.Identity.Identity
--     (Result b)
-- bbb =

data State = State
  { _browser :: Browser
  }

makeLenses ''State

data Name
  = BrowserName Browser.Name
  deriving (Show, Eq, Ord)

data Event
  = BrowserEvent Browser.Event

app :: App State Event Name
app =
  App
    { appDraw = draw,
      appChooseCursor = Focus.focusRingCursor getFocusRing,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const myAttrMap
    }

myAttrMap :: AttrMap
myAttrMap =
  applyAttrMappings Banner.attrs $
    Brick.attrMap
      V.defAttr
      [ (BrickList.listSelectedAttr, V.black `on` V.white),
        ("geminiH1", V.currentAttr `withStyle` bold `withStyle` underline),
        ("geminiH2", V.currentAttr `withStyle` underline),
        ("geminiH3", V.currentAttr `withStyle` bold),
        ("geminiUriUnknown", fg blue),
        ("geminiUriLoading", fg yellow),
        ("geminiUriLoaded", fg green),
        ("geminiUriErrored", fg red),
        ("yellow", fg yellow),
        ("geminiPre", fg blue),
        (Dialog.buttonAttr, fg white),
        (Dialog.buttonSelectedAttr, black `on` yellow)
      ]

getFocusRing :: State -> Focus.FocusRing Name
getFocusRing s = undefined

-- s ^. browser ^. Browser.focusRing

draw :: State -> [Widget Name]
draw s = undefined

--fmap BrowserName (s ^. browser & Browser.draw)

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s e = undefined

-- continue $ s {_browser = Browser.handleEvent (s ^. browser) e}

handleVtyEvent :: State -> V.Event -> EventM Name (Next State)
handleVtyEvent state event =
  case event of
    (V.EvKey (V.KChar 'c') [V.MCtrl]) ->
      halt state

-- _ ->
--   state & browser %~ Browser.handleVtyEvent event

initState :: Browser -> State
initState browser =
  State
    { _browser = browser
    }

runApp :: String -> IO ()
runApp initUri = do
  undefined

-- let buildVty = V.mkVty V.defaultConfig
-- initialVty <- buildVty
-- eventChan <- BChan.newBChan 10
-- browser <- Browser.init initUri (BChan.writeBChan eventChan)
-- let state = initState browser
-- void $ customMain initialVty buildVty (Just eventChan) app state

main :: IO ()
main =
  runApp "gemini://gemini.circumlunar.space/"
