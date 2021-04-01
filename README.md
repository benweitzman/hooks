# hooks

FRP-ish / Reactive programming inspired by React hooks

```haskell
{-# LANGUAGE QualifiedDo #-}

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

  once $ do -- read a number from the command line
    thread <- async . forever $ do
      x <- getLine
      updateNum . Set $ read x
    return $ cancel thread

  let idxs = (\num -> [1..num]) <$> numState

  useMap idxs $ \i -> Hook.do
    (randomOffset, updateRandomOffset) <- useState @Int 0
    once $ do
      randomValue <- randomRIO (-1000000, 1000000)
      updateRandomOffset $ Set randomValue
      return $ return ()
    useTick $ (1000000 +) <$> randomOffset
```

This example runs a bunch of counters in parallel. You can change the number
by entering numbers on the command line.

[![asciicast](https://asciinema.org/a/D63N4XhgMr7dxduVg8sXql3Kh.svg)](https://asciinema.org/a/D63N4XhgMr7dxduVg8sXql3Kh)

`Hooks` is a not-quite-Monad for writing programs producing values
that change over time. Support for `RebindableSyntax` or `QualifiedDo` included.


## Writing programs with hooks

Hooks programs produce streams of values. As internal state changes, the body of 
the program is run over and over to produce successive outputs. 

Every hooks program is built up from a few primitives:

* The `State` primitive yields a stateful value and gives an imperative handle for modifying the state.    
* The `Effect` primitive will execute code in `IO` whenever its first argument ("dependencies" in React parlance) 
  changes.   
* The `Map` primitive takes a list of inputs and creates a subcontext for each one. 
  As values are produced in the subcontexts, they are bundled up together producing a list of results. 

  This is analogous to `sequence` from `Traversable`. 
* The `Case` primitive makes it possible to branch on stateful values.    


If you're familiar with React hooks, `State` and `Effect`, are direct analogs of `useState` and `useEffect` and work 
identically. 

`Map` and `Case` are unique to this Haskell library. In React, the hook rules forbid iterating over an array and calling
 hooks on each element. The React way would be to create sub components, calling hooks from the sub components and 
 avoiding breaking the rules. In Haskell, we don't have components, or even a DOM, so `Map` gives us a way to 
 iterate over a dynamic list. Like lists of React Components that use the `key` property, `Map` will preserve the states
 of the subcontexts even as they are reordered.
 
## What about the Rules of Hooks?

`Hooks` is fully type safe, and so a large class of bugs in React programs are actually unrepresentable. In React, as in
this library, the behavior of programs that use hooks is dependent on the order in which different hooks are called. However,
unlike React, this library encodes the sequence of hooks used at the type level, and so it's impossible to write
a program that would use different orders of hooks during different executions. Because `Hooks` tracks the order of effects,
it's not quite a `Monad`. This precludes you from using common `Monad` functions like `when` or `forM`, but that's not so
bad as these functions generally correspond to ways you can break the Rules Of Hooks. 

Instead of these functions, branching should be handled using `useMap` and `useCase` so that it's visible to the library.

Additionally, values returned from stateful computations are tagged as such. The only way to untag them is to pass them
to `useEffect`, `useCase` or `useMap`, which ensures that all dependencies (again, using React parlance) are accounted for.

I like to use the `PartialTypeSignatures` extension and let GHC infer the sequence of hooks I'm using, because it doesn't 
really matter that much what it is. The only thing that matters is that it has *some* type which gives us confidence
that our program isn't branching (without `useCase`). Of course, feel free to fully specify the sequence yourself if
you'd like. As an example, `testProg` above has the actual type: 

```haskell
testProg :: Hooks IO [ State Int, Effect ()
                     , Map '[State Int, Effect (), Context "tick"] Int Int
                     ] [Int]
```

## Why? 

I wanted to write some programs that use websockets to push data from several sources to web browsers. FRP libraries 
out there are powerful, but I don't want to think of my program in terms of `Events` and `Behaviors`. `conduit` could
probably help, but I couldn't make it through all the type parameters. This library gives a simple interface for 
composing lots of stateful values.

Also I wanted an excuse to try out `QualifiedDo` :p 

