module Loadable where

data Loadable a
  = NotStarted
  | Loading
  | Loaded a
  | NetErr
  deriving (Show, Eq)

isReady :: Loadable a -> Bool
isReady loadable =
  case loadable of
    Loaded _ -> True
    _ -> False

isResolved :: Loadable a -> Bool
isResolved (Loaded _) = True
isResolved (NetErr) = True
isResolved _ = False

isLoading :: Loadable a -> Bool
isLoading Loading = True
isLoading _ = False
