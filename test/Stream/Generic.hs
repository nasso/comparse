{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Stream.Generic (genericStreamTests) where

import Data.Stream (Stream (Item, next))
import Test.Tasty
import Test.Tasty.HUnit

genericStreamTests ::
  (Stream s, Show s, Eq s, Item s ~ Char) =>
  (Int -> Int -> String -> s) ->
  [TestTree]
genericStreamTests makeAt =
  [ testCase "next on same line gives next character" $
      next (make "foo\nbar\nbaz") @?= Just ('f', makeAt 0 1 "oo\nbar\nbaz"),
    testCase "next at end of line gives linefeed" $
      next (make "\nbar\nbaz") @?= Just ('\n', makeAt 1 0 "bar\nbaz"),
    testCase "next before end of line stops before linefeed" $
      next (make "o\nbar\nbaz") @?= Just ('o', makeAt 0 1 "\nbar\nbaz"),
    testCase "next at end of file gives Nothing" $
      next (make "") @?= Nothing
  ]
  where
    make = makeAt 0 0
