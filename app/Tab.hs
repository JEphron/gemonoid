{-# LANGUAGE TemplateHaskell #-}

module Tab where

import Data.Vector (Vector)
import History (History)
import qualified History
import Lens.Micro.Platform
import Page (Page)
import qualified Page

data Tab = Tab
  { _page :: Page,
    _history :: History
  }
  deriving (Show)

makeLenses ''Tab

new :: Tab
new =
  Tab Page.blank History.empty

show :: Page -> History.Behavior -> Tab -> Tab
show newPage historyBehavior tab =
  let oldPage = tab ^. page ^. Page.uri
   in case historyBehavior of
        History.Push ->
          tab
            & page .~ newPage
            & history %~ History.push oldPage
        History.Replace ->
          tab
            & page .~ newPage
            & history %~ History.replace oldPage
        History.Cache ->
          error "todo"
