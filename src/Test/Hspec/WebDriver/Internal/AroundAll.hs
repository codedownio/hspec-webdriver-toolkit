-- | Perform an action around the set of tests.
-- This module based on @chrisdone's comment at
-- https://github.com/hspec/hspec/issues/255#issuecomment-585664769
module Test.Hspec.WebDriver.Internal.AroundAll where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Test.Hspec
import Test.Hspec.WebDriver.Internal.Misc

-- | Wrap an action around the given spec.
aroundAll :: (ActionWith a -> IO ()) -> SpecWith a -> Spec
aroundAll actionWithAToUnit = aroundAllWith (const . actionWithAToUnit)

-- | Wrap an action around the given spec. Changes the arg type inside.
aroundAllWith :: (ActionWith a -> ActionWith b) -> SpecWith a -> SpecWith b
aroundAllWith actionWithAToB specWithA = do
  done <- runIO newEmptyMVar
  beforeAllWith'
    (\b -> do
       -- The mutable reference is needed because @actionWithAToB@
       -- returns a (), not an @a@, as we'd like.
       mvarA <- newEmptyMVar
       -- The async thread is needed because @actionWithAToB@
       -- allocates and then /frees/ the resource, so we should block
       -- it from doing so until @afterAll@ has executed. Hence the
       -- @done@ var.
       blocker <-
         async
           (actionWithAToB
              (\a -> do
                 -- We make the @a@ value immediately available to the
                 -- @takeMVar@ below, to be passed to @specWithA@ down
                 -- the road.
                 putMVar mvarA a
                 -- Wait for the test to be done before allowing
                 -- @actionWithAToB@ to clean up resources.
                 takeMVar done)
              b)
       -- If @actionWithAToB@ fails then it should rethrow the
       -- exception to this thread before we try to @takeMVar@ on a
       -- var that will never be filled. This gives better error
       -- messages than a "blocked indefinitely".
       --
       -- Link is non-blocking.
       link blocker
       -- This should block until the @a@ is allocated.
       takeMVar mvarA)
    (afterAll (const (putMVar done ())) specWithA)
