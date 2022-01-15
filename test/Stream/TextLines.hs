module Stream.TextLines (textStreamTests) where

import Data.Stream
import Data.Stream.TextLines (TextLines (..), TextPos (TextPos))
import qualified Data.Text as T
import qualified Data.Vector as V
import Stream.Generic (genericStreamTests)
import Test.Tasty
import Test.Tasty.HUnit

textStreamTests :: TestTree
textStreamTests =
  testGroup "Text" $
    testCase
      "getPos returns current position"
      (getPos (makeAt 3 5 "foo\nbar") @?= makePosAt 3 5 "foo\nbar") :
    genericStreamTests makeAt
  where
    makeAt l c s =
      let t = T.pack s
       in TextLines t $ TextPos l c $ V.fromList $ T.lines t
    makePosAt l c s = TextPos l c $ V.fromList $ T.lines $ T.pack s
