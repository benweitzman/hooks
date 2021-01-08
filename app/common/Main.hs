module Main where

import Examples
import Control.Monad.Hooks
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  HookExecutionHandle { await } <- runHooks testProg $ \v -> do
    putStr $ "\r" ++ show v
    hFlush stdout
  await
