module Control.Monad.Hooks
  ( -- * Running Hooks
    runHooks
  , HookExecutionHandle(..)
  , Stateful
  , Escaping(..)
  , AndEscaping(..)
  -- * State
  -- | The State hook is used to get and set internal state of a program.
  -- Each call to `useState` manages an atomic piece of state, and through
  -- multiple calls, these states can be composed together to create a complex
  -- system. 
  -- 
  -- Every bit of state is initialized with a starting value, which can be determined
  -- purely, using `useState` or synchronously via an impure getter, using `useStateSync`.
  -- Asynchronous state initialization can be modeled using `useState Nothing` combined
  -- with a call to `useEffect`. 
  , useState
  , useStateSync
  , StateUpdate(..)
  -- * Effect
  , useEffect
  , once
  -- * Ref
  , useRef
  -- * Context
  , useContext
  -- * Map
  , useMap
  -- * Case
  , useCase
  , Clause(When, LetWhen)
  , Clauses(Or, Else)
  , (->>)
  , (->>*)
  -- * Writing your own primitives
  , module X
  ) where

import Control.Monad.Hooks.Primitives as X
import Control.Monad.Hooks.Class as X
import Control.Monad.Hooks.Runtime as X
import Control.Monad.Hooks.Stateful as X (Stateful, Escaping(..), AndEscaping(..))