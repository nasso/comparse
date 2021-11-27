module Stream (streamTests) where

import Stream.StringLines (stringStreamTests)
import Test.Tasty

streamTests :: [TestTree]
streamTests = [stringStreamTests]
