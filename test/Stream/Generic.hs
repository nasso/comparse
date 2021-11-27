{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Stream.Generic (genericStreamTests) where

import Data.Stream
import Data.String (IsString, fromString)
import Test.Tasty
import Test.Tasty.HUnit

genericStreamTests ::
  ( Stream s
  , Show s
  , Eq s
  , Item s ~ Char
  , IsString (Chunk s)
  , Eq (Chunk s)
  , Show (Chunk s)
  ) =>
  (Int -> Int -> String -> s) ->
  [TestTree]
genericStreamTests makeAt =
  [ testCase "next on same line gives next character" $
      next (make "foo\nbar\nbaz") @?= Just ('f', makeAt 0 1 "oo\nbar\nbaz")
  , testCase "next at end of line gives linefeed" $
      next (make "\nbar\nbaz") @?= Just ('\n', makeAt 1 0 "bar\nbaz")
  , testCase "next before end of line stops before linefeed" $
      next (make "o\nbar\nbaz") @?= Just ('o', makeAt 0 1 "\nbar\nbaz")
  , testCase "next at end of file gives Nothing" $
      next (make "") @?= Nothing
  , testCase "nextWhile gives empty chunk when nothing matches" $
      nextWhile (== 'x') (make "foo") @?= (fromString "", makeAt 0 0 "foo")
  , testCase "nextWhile gives matching chunk" $
      nextWhile (== 'o') (make "oof") @?= (fromString "oo", makeAt 0 2 "f")
  , testCase "nextWhile can consume all stream" $
      nextWhile (const True) (make "foo") @?= (fromString "foo", makeAt 0 3 "")
  , testCase "nextWhile can consume multiple lines" $
      nextWhile (const True) (make "foo\nbar\nbaz")
        @?= (fromString "foo\nbar\nbaz", makeAt 2 3 "")
  , testCase "nextN consume nothing when N=0" $
      nextN 0 (make "foo") @?= (fromString "", makeAt 0 0 "foo")
  , testCase "nextN consume one character when N=1" $
      nextN 1 (make "foo") @?= (fromString "f", makeAt 0 1 "oo")
  , testCase "nextN consume multiple characters when N>1" $
      nextN 3 (make "foo") @?= (fromString "foo", makeAt 0 3 "")
  , testCase "nextN stops at end of file" $
      nextN 3 (make "") @?= (fromString "", makeAt 0 0 "")
  , testCase "nextN counts linefeeds" $
      nextN 2 (make "\nfoo\nbar") @?= (fromString "\nf", makeAt 1 1 "oo\nbar")
  ]
 where
  make = makeAt 0 0
