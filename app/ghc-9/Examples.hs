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

{-

(numState, updateNum) <- useState (0 :: Int)

once $ do
  thread <- async . forever $ do
    x <- getLine
    updateNum . Set $ read x
  return $ cancel thread


let idxs = flip fmap numState $ \num -> [1..num]

useMap idxs $ \i -> Hook.do
  (randomOffsetState, updateRandomOffset) <- useState 0
  once $ do
    randomValue <- randomRIO (-1000000, 1000000)
    updateRandomOffset $ Set randomValue
    return $ return ()

  (tickState, setTick) <- useState 0

  useEffect (randomOffsetState, numState) $ \interval -> do
    thread <- async $ forever $ do
      threadDelay interval
      setTick $ Modify (+1)
    return $ cancel thread

  useTick $ (1000000 +) <$> randomOffset
-}

useTick :: Stateful Int -> Hooks IO _ (Stateful Int)
useTick intervalState = useContext @"tick" $ Hook.do
  (tickState, setTick) <- useState 0

  useEffect intervalState $ \interval -> do
    thread <- async $ forever $ do
      threadDelay interval
      setTick $ Modify (+1)
    return $ cancel thread

  Hook.return tickState

testProg :: Hooks IO _ [Stateful Int]
testProg = Hook.do
  (numState, updateNum) <- useState (0 :: Int)

  once $ do
    thread <- async . forever $ do
      x <- getLine
      updateNum . Set $ read x
    return $ cancel thread

  let idxs = flip fmap numState $ \num -> [1..num]

  useMap (Stateful ()) idxs $ \() i -> Hook.do
    (randomOffset, updateRandomOffset) <- useState 0
    once $ do
      randomValue <- randomRIO (-1000000, 1000000)
      updateRandomOffset $ Set randomValue
      return $ return ()
    useTick ((1000000 +) <$> randomOffset)