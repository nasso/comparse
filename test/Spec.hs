import Json
import Parsing
import Stream
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ testGroup "Streams" streamTests
      , testGroup "Parsers" parserTests
      , testGroup "Json" jsonTests
      ]
