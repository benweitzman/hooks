{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QualifiedDo #-}

module Examples where

import Control.Monad.Hooks
import qualified Control.Monad.Hooks.Do.Qualified as Hook
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

useTick :: Int -> Hooks _ Int
useTick interval = Hook.do
  (tick, setTick) <- Use (State 0)

  Use $ Effect interval $ do
    thread <- async $ forever $ do
      threadDelay interval
      setTick $ Modify (+1)
    return $ cancel thread

  Hook.return tick

testProg :: Hooks _ [Int]
testProg = Hook.do
  (num, updateNum) <- Use $ State (0 :: Int)

  Use $ Effect () $ do
    thread <- async . forever $ do
      x <- getLine
      updateNum . Set $ read x
    return $ cancel thread

  Use $ Map () [1..num] $ \i -> Hook.do
    (randomOffset, updateRandomOffset) <- Use $ State 0
    Use $ Effect () $ do
      randomValue <- randomRIO (-1000000, 1000000)
      updateRandomOffset $ Set randomValue
      return $ return ()
    useTick (1000000 + randomOffset)