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

useTick :: Stateful Int -> Hooks IO _ Int
useTick intervalState = useContext @"tick" $ let Do.Hook {..} = Do.hook in do
  (tickState, setTick) <- useState 0

  useEffect intervalState $ \interval -> let Do.Monad {..} = Do.monad in do
    thread <- async $ forever $ do
      threadDelay interval
      setTick $ Modify (+1)
    return $ cancel thread

  HookReturn tickState

testProg :: Hooks IO _ [Int]
testProg = let Do.Hook {..} = Do.hook in do

  (numState, updateNum) <- useState 0

  once $ let Do.Monad {..} = Do.monad in do
    thread <- async . forever $ do
      x <- getLine
      updateNum . Set $ read x
    return $ cancel thread

  let idxs = let Do.Monad {..} = Do.monad in do
        num <- numState
        return [1..num]

  useMap idxs $ \i -> do
    (randomOffset, updateRandomOffset) <- useState 0

    once $ let Do.Monad {..} = Do.monad in do
      randomValue <- randomRIO (-1000000, 1000000)
      updateRandomOffset $ Set randomValue
      return $ return ()

    t <- useTick $ (1000000 +) <$> randomOffset

    modifier <- useCase t $ let Do.Branch {..} = Do.branch in do
      When even $ let Do.Hook {..} = Do.hook in do
        useTick (pure 50000)
      When odd $ let Do.Hook {..} = Do.hook in do
        t' <- useTick (pure 50000)
        return $ negate <$> t'
      Else (1 :: Int)

    return $ let Do.Monad {..} = Do.monad in do
      m <- modifier
      v <- t
      return $ m * v

