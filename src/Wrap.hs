{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}

module Wrap where

import Test.Hspec.Core.Spec


labelify :: SpecWith [String] -> Spec
labelify = addLabelsToTree (\labels () -> labels)

addLabelsToTree :: forall a b. ([String] -> a -> b) -> SpecWith b -> SpecWith a
addLabelsToTree transform specA = do
  trees <- runIO (runSpecM specA)
  fromSpecList $ map (transformTree []) trees

  where
    transformTree :: [String] -> SpecTree b -> SpecTree a
    transformTree labels (Node label innerTrees) = Node label (fmap (transformTree (label : labels)) innerTrees)
    transformTree labels leaf@(Leaf item@(Item {itemRequirement, itemExample})) = Leaf (item {itemExample=newExample})
      where newExample params newFn db = itemExample params (act newFn) db
            act :: (ActionWith a -> IO ()) -> ActionWith b -> IO ()
            act newFn actionExpectingB = newFn $ \x -> actionExpectingB (transform (itemRequirement : labels) x)
