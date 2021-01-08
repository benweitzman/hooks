{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}

module Examples where

import Control.Monad.Hooks
import qualified Control.Monad.Hooks.Do as Do
import Prelude
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

useTick :: Int -> Hooks _ Int
useTick interval = let Do.Hook {..} = Do.hook in do
  (tick, setTick) <- Use (State 0)

  Use $ Effect interval $ let Do.Monad {..} = Do.monad in do
    thread <- async $ forever $ do
      threadDelay interval
      setTick $ Modify (+1)
    return $ cancel thread

  HookReturn tick

testProg :: Hooks _ [Int]
testProg = let Do.Hook {..} = Do.hook in do

  (num, updateNum) <- Use $ State (0 :: Int)

  Use $ Effect () $ let Do.Monad {..} = Do.monad in do
    thread <- async . forever $ do
      x <- getLine
      updateNum . Set $ read x
    return $ cancel thread

  Use $ Map () [1..num] $ \i -> do
    (randomOffset, updateRandomOffset) <- Use $ State 0

    Use $ Effect () $ let Do.Monad {..} = Do.monad in do
      randomValue <- randomRIO (-1000000, 1000000)
      updateRandomOffset $ Set randomValue
      return $ return ()
    useTick (1000000 + randomOffset)