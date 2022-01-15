module Stream (streamTests) where

import Stream.StringLines (stringStreamTests)
import Stream.TextLines (textStreamTests)
import Test.Tasty

streamTests :: [TestTree]
streamTests = [stringStreamTests, textStreamTests]
