{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QualifiedDo #-}

module Examples where

import Control.Monad.Hooks
import qualified Control.Monad.Hooks.Do.Qualified as Hook
import qualified Control.Monad.Hooks.Branch as Branch
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

useTick :: Num a => Stateful Int -> Hooks IO '[Context "tick"] a
useTick intervalState = useContext @"tick" $ Hook.do
  (tickState, setTick) <- useState 0

  useEffect intervalState $ \interval -> do
    thread <- async $ forever $ do
      threadDelay interval
      setTick $ Modify (+1)
    return $ cancel thread

  Hook.return tickState

testProg :: Hooks IO _ [Int]
testProg = Hook.do
  (numState, updateNum) <- useState (0 :: Int)

  once $ do
    thread <- async . forever $ do
      x <- getLine
      updateNum . Set $ read x
    return $ cancel thread

  let idxs = do
        num <- numState
        return [1..num]

  counts <- useMap idxs $ \i -> Hook.do
    (randomOffset, updateRandomOffset) <- useState 0

    once $ do
      randomValue <- randomRIO (-1000000, 1000000)
      updateRandomOffset $ Set randomValue
      return $ return ()

    t <- useTick @Int ((1000000 +) <$> randomOffset)

    modifier <- useCase t $ Branch.do
      even ->> Hook.do
        useTick (return 50000)
      odd ->> Hook.do
        t' <- useTick @Int (return 50000)
        Hook.return $ negate <$> t'
      Else (1 :: Int)

    Hook.return $ do
      m <- modifier
      v <- t
      return $ m * v

  Hook.return $ counts