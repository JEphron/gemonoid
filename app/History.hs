module History where

import Data.Coerce (coerce)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.URI (URI)
import qualified Utils

data Behavior
  = Push
  | Replace
  | Cache
  deriving (Eq, Show)

data History
  = History (Vector URI)
  deriving (Show)

empty :: History
empty = History Vector.empty

push :: URI -> History -> History
push uri (History history) =
  History (Vector.snoc history uri)

pop :: History -> (History, Maybe URI)
pop (History history) =
  let (vec, popped) = Utils.pop history
   in (History vec, popped)

replace :: URI -> History -> History
replace uri history =
  let (history', _) = pop history
   in push uri history'
