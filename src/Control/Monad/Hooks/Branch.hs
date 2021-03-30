module Control.Monad.Hooks.Branch where

import Control.Monad.Hooks.Primitives.Case 

(>>) :: Clause m a b x -> Clauses m a b ys -> Clauses m a b (x : ys)
(>>) = Or