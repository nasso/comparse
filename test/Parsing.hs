module Parsing where

import Control.Monad.Parser
import qualified Data.List.NonEmpty as NonEmpty
import Data.Stream.StringLines
import Test.Tasty
import Test.Tasty.HUnit

parse :: StringParser a -> String -> Maybe (a, String)
parse p s =
    case runStringParser p s of
        Parsed v (StringLines rest _) _ -> Just (v, rest)
        _ -> Nothing

parserTests :: [TestTree]
parserTests =
    [ coreParserTests
    , coreCombinatorsTests
    , derivedParserTests
    , derivedCombinatorsTests
    ]

coreParserTests :: TestTree
coreParserTests =
    testGroup
        "Core parsers"
        [ testCase "item returns next char" $
            parse item "hi" @?= Just ('h', "i")
        , testCase "item fails on eof" $
            parse item "" @?= Nothing
        , testCase "eof succeeds on eof" $
            parse eof "" @?= Just ((), "")
        , testCase "eof fails on non-eof" $
            parse eof "hi" @?= Nothing
        , testCase "noParse fails with non-empty input" $
            parse (noParse :: StringParser ()) "hi" @?= Nothing
        , testCase "noParse fails with empty input" $
            parse (noParse :: StringParser ()) "" @?= Nothing
        ]

coreCombinatorsTests :: TestTree
coreCombinatorsTests =
    testGroup
        "Core combinators"
        [ testCase "followedBy fails when parser fails" $
            parse (followedBy $ like 'h') "ello" @?= Nothing
        , testCase "followedBy succeeds without consuming input" $
            parse (followedBy $ like 'h') "hi" @?= Just ((), "hi")
        , testCase "notFollowedBy fails when parser succeeds" $
            parse (notFollowedBy $ like 'h') "hi" @?= Nothing
        , testCase "notFollowedBy succeeds without consuming input" $
            parse (notFollowedBy $ like 'g') "hi" @?= Just ((), "hi")
        , testCase "`a <|> b` returns a when it succeeds" $
            parse (like 'h' <|> like 'g') "hi" @?= Just ('h', "i")
        , testCase "`a <|> b` returns b when a fails" $
            parse (like 'g' <|> like 'h') "hi" @?= Just ('h', "i")
        , testCase "`a <|> b` fails when both fail" $
            parse (like 'g' <|> like 'i') "hi" @?= Nothing
        ]

derivedParserTests :: TestTree
derivedParserTests =
    testGroup
        "Derived parsers"
        [ testCase "like succeeds on matching item" $
            parse (like 'h') "hi" @?= Just ('h', "i")
        , testCase "like fails on non-matching item" $
            parse (like 'h') "pi" @?= Nothing
        , testCase "like fails on eof" $
            parse (like 'h') "" @?= Nothing
        , testCase "unlike succeeds on non-matching item" $
            parse (unlike 'g') "hi" @?= Just ('h', "i")
        , testCase "unlike fails on matching item" $
            parse (unlike 'h') "hi" @?= Nothing
        , testCase "string succeeds on matching string" $
            parse (string "hell") "hello" @?= Just ("hell", "o")
        , testCase "string fails on non-matching string" $
            parse (string "hello") "hi" @?= Nothing
        , testCase "`string \"\"` succeeds on arbitrary input" $
            parse (string "") "hi" @?= Just ("", "hi")
        , testCase "`string \"\"` succeeds on empty input" $
            parse (string "") "" @?= Just ("", "")
        ]

derivedCombinatorsTests :: TestTree
derivedCombinatorsTests =
    testGroup
        "Derived combinators"
        [ testCase "satisfy succeeds on matching char" $
            parse (satisfy item (== 'h')) "hi" @?= Just ('h', "i")
        , testCase "satisfy fails on non-matching char" $
            parse (satisfy item (== 'g')) "hi" @?= Nothing
        , testCase "optional returns Nothing on eof" $
            parse (optional item) "" @?= Just (Nothing, "")
        , testCase "optional returns Nothing on failure" $
            parse (optional $ like 'g') "hi" @?= Just (Nothing, "hi")
        , testCase "optional returns Just on success" $
            parse (optional item) "hi" @?= Just (Just 'h', "i")
        , testCase "many returns empty list on eof" $
            parse (many item) "" @?= Just ([], "")
        , testCase "many returns list of items" $
            parse (many item) "hi" @?= Just ("hi", "")
        , testCase "many1 fails on eof" $
            parse (many1 item) "" @?= Nothing
        , testCase "many1 returns non-empty list of items" $
            parse (many1 item) "hi" @?= Just ("hi", "")
        , testCase "some fails on eof" $
            parse (some item) "" @?= Nothing
        , testCase "some returns NonEmpty list of items" $
            parse (some item) "hi" @?= Just (NonEmpty.fromList "hi", "")
        , testCase "choice takes first successful parser" $
            parse (choice [like 'h', like 'g']) "hi" @?= Just ('h', "i")
        , testCase "choice with empty parser list fails" $
            parse (choice [] :: StringParser ()) "hi" @?= Nothing
        , testCase "choice with no matching parser fails" $
            parse (choice [like 'g', like 'j']) "hi" @?= Nothing
        , testCase "sepBy matches empty list" $
            parse (sepBy item $ like ',') "" @?= Just ([], "")
        , testCase "sepBy matches list of one item" $
            parse (sepBy item $ like ',') "hi" @?= Just ("h", "i")
        , testCase "sepBy matches list of many items" $
            parse (sepBy item $ like ',') "h,e,llo" @?= Just ("hel", "lo")
        , testCase "sepBy doesn't consume trailing separator" $
            parse (sepBy item $ like ',') "h,i," @?= Just ("hi", ",")
        , testCase "sepBy1 doesn't match empty list" $
            parse (sepBy1 item $ like ',') "" @?= Nothing
        , testCase "sepBy1 matches list of one item" $
            parse (sepBy1 item $ like ',') "hi"
                @?= Just (NonEmpty.fromList "h", "i")
        , testCase "sepBy1 matches list of many items" $
            parse (sepBy1 item $ like ',') "h,e,llo"
                @?= Just (NonEmpty.fromList "hel", "lo")
        , testCase "sepBy1 doesn't consume trailing separator" $
            parse (sepBy1 item $ like ',') "h,i,"
                @?= Just (NonEmpty.fromList "hi", ",")
        , testCase "oneOf fails when item doesn't match any" $
            parse (oneOf "gj") "hi" @?= Nothing
        , testCase "oneOf succeeds when item matches one" $
            parse (oneOf "gh") "hi" @?= Just ('h', "i")
        , testCase "oneOf succeeds when item matches many" $
            parse (oneOf "ghh") "hi" @?= Just ('h', "i")
        , testCase "oneOf fails with empty class" $
            parse (oneOf "") "hi" @?= Nothing
        , testCase "noneOf succeeds when item doesn't match any" $
            parse (noneOf "gj") "hi" @?= Just ('h', "i")
        , testCase "noneOf fails when item matches one" $
            parse (noneOf "gh") "hi" @?= Nothing
        , testCase "noneOf fails when item matches many" $
            parse (noneOf "ghh") "hi" @?= Nothing
        , testCase "noneOf succeeds with empty class" $
            parse (noneOf "") "hi" @?= Just ('h', "i")
        ]
