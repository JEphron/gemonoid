module Cache where

import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro.Platform hiding (set)
import Network.URI (URI)
import qualified Network.URI as URI
import Page (Page)
import qualified Page as Page

newtype Cache = Cache (Map URI Page) deriving (Show)

setLoading :: URI -> Cache -> Cache
setLoading uri =
  set (Page.loading uri)

set :: Page -> Cache -> Cache
set page =
  coerce (Map.insert (page ^. Page.uri) page)

get :: URI -> Cache -> Maybe Page
get uri =
  Map.lookup uri . coerce

missing :: URI -> Cache -> Bool
missing uri (Cache c) =
  Map.notMember uri c

new :: Cache
new =
  Cache (Map.empty)

count :: Cache -> Int
count (Cache c) = Map.size c

countWhere :: (Page -> Bool) -> Cache -> Int
countWhere p (Cache c) =
  Map.size $ (Map.filter p) c
