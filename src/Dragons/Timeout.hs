{-# LANGUAGE BangPatterns, LambdaCase #-}
{-|
Module      : Dragons.Timeout
Description : Take the last value produced within a given time
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3

Take the last element of a lazily generated list generated within a given 
time.
-}

module Dragons.Timeout (timeoutTake) where

import Data.IORef
import System.Timeout

-- To avoid dependencies, we roll a small version of NFData here

-- | A type which can be evaluated to normal form
class NF a where
  toNF :: a -> ()

instance NF () where
  toNF () = ()

instance NF Int where
  toNF !_ = ()

instance (NF a, NF b) => NF (a,b) where
  toNF (a,b) = toNF a `seq` toNF b

-- | Given a function which lazily produces a list,
-- take the last value which was produced before the timeout
-- return `Nothing` if no value was produced in time
timeoutTake :: NF a => Double -> [a] -> IO (Maybe a)
timeoutTake time list = do
  let timeLimit = round (time * 1000000)
  lastResult <- newIORef Nothing
  let go = \case
        [] -> return Nothing
        [x] -> toNF x `seq` return (Just x)
        x:xs -> toNF x `seq` do
          writeIORef lastResult (Just x)
          go xs
  timeout timeLimit (go list) >>= \case
    Nothing -> readIORef lastResult
    Just result -> return result


