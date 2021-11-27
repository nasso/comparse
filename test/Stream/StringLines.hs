module Stream.StringLines (stringStreamTests) where

import Data.Stream
import Data.Stream.StringLines (StringLines (..), StringPos (StringPos))
import qualified Data.Stream.StringLines as StringLines
import Stream.Generic (genericStreamTests)
import Test.Tasty
import Test.Tasty.HUnit

stringStreamTests :: TestTree
stringStreamTests =
  testGroup "String" $
    [ testCase "fromString" $
        StringLines.fromString "foo\nbar\nbaz"
          @?= StringLines "foo\nbar\nbaz" (StringPos 0 0 "foo")
    , testCase "getPos returns current position" $
        getPos (makeAt 3 5 "foo\nbar") @?= StringPos 3 5 "foo"
    ]
      ++ genericStreamTests makeAt
 where
  makeAt l c s = StringLines s $ StringPos l c $ takeWhile (/= '\n') s
