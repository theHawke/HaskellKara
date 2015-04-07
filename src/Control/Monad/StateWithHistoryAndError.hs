module Control.Monad.StateWithHistoryAndError (
    StateWithHistoryAndError
  , Runningstate
  , get
  , put
  , modify
  , failure
  , getHistory
  , getEndReason
  , getEndResult
  ) where

import Control.Monad (ap)

type RunningState s e a = (Either e (a, s), [s])

newtype StateWithHistoryAndError s e a = SWHE { runState :: s -> RunningState s e a }

instance Functor (StateWithHistoryAndError s e) where
  fmap f (SWHE rs) = SWHE $ \s -> case rs s of
    (Right (a, s), h) -> (Right (f a, s), h)
    (Left e, h) -> (Left e, h)

instance Applicative (StateWithHistoryAndError s e) where
  pure  = return
  (<*>) = ap

instance Monad (StateWithHistoryAndError s e) where
  return a = SWHE $ \s -> (Right (a, s), [])
  m >>= k = SWHE $ \s -> case runState m s of
    (Left e, h)        -> (Left e, h)
    (Right (a, s'), h) -> let
      (es, h') = runState (k a) s'
      in (es, h ++ h')

get :: StateWithHistoryAndError s e s
get = SWHE (\s -> (Right (s, s), []))

put :: s -> StateWithHistoryAndError s e ()
put new = SWHE (\old -> (Right ((), new), [old]))

modify :: (s -> s) -> StateWithHistoryAndError s e ()
modify f = SWHE $ \s -> (Right ((), f s), [s])

failure :: e -> StateWithHistoryAndError s e a
failure e = SWHE $ \s -> (Left e, [s])

getHistory :: RunningState s e a -> [s]
getHistory (r, h) = h ++ (case r of Left e -> []; Right (_, s') -> [s'])

getEndReason :: RunningState s e a -> Maybe e
getEndReason (r, _) = case r of Right _ -> Nothing; Left e -> Just e

getEndResult :: RunningState s e a -> Either e a
getEndResult (r, _) = case r of Left e -> Left e; Right (a, _) -> Right a
