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

useTick :: Num a => Int -> Hooks IO _ a
useTick interval = useNamedContext @"tick" $ Hook.do
  (tick, setTick) <- useState 0

  useEffect interval $ do
    thread <- async $ forever $ do
      threadDelay interval
      setTick $ Modify (+1)
    return $ cancel thread

  Hook.return tick

testProg :: Hooks IO _ [Int]
testProg = Hook.do
  (num, updateNum) <- useState (0 :: Int)

  once $ do
    thread <- async . forever $ do
      x <- getLine
      updateNum . Set $ read x
    return $ cancel thread

  useMap () [1..num] $ \i -> Hook.do
    (randomOffset, updateRandomOffset) <- useState 0
    once $ do
      randomValue <- randomRIO (-1000000, 1000000)
      updateRandomOffset $ Set randomValue
      return $ return ()
    useTick (1000000 + randomOffset)