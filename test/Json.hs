module Json (jsonTests) where

import Control.Monad (void)
import Control.Monad.Parser
import Data.Char (isAlpha, isDigit)
import Data.Stream.StringLines
import Test.Tasty
import Test.Tasty.HUnit

data JValue
  = JString String
  | JNumber Int
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Show)

jsonTests :: [TestTree]
jsonTests =
  [ testCase "string" $ parseJValue "\"foo\"" @?= Just (JString "foo"),
    testCase "empty string" $ parseJValue "\"\"" @?= Just (JString ""),
    testCase "number" $ parseJValue "123" @?= Just (JNumber 123),
    testCase "negative number" $ parseJValue "-123" @?= Just (JNumber (-123)),
    testCase "positive number" $ parseJValue "+123" @?= Just (JNumber 123),
    testCase "bool true" $ parseJValue "true" @?= Just (JBool True),
    testCase "bool false" $ parseJValue "false" @?= Just (JBool False),
    testCase "null" $ parseJValue "null" @?= Just JNull,
    testCase "object" $
      parseJValue "{\"foo\": \"bar\"}"
        @?= Just (JObject [("foo", JString "bar")]),
    testCase "empty object" $ parseJValue "{}" @?= Just (JObject []),
    testCase "array" $
      parseJValue "[1, 2, 3]"
        @?= Just (JArray [JNumber 1, JNumber 2, JNumber 3]),
    testCase "empty array" $ parseJValue "[]" @?= Just (JArray []),
    testCase "nested array" $
      parseJValue "[1, [2, 3], 4]"
        @?= Just (JArray [JNumber 1, JArray [JNumber 2, JNumber 3], JNumber 4]),
    testCase "nested object" $
      parseJValue "{\"foo\": {\"bar\": \"baz\"}}"
        @?= Just (JObject [("foo", JObject [("bar", JString "baz")])]),
    testCase "nested object with array" $
      parseJValue "{\"foo\": {\"bar\": [1, 2, 3]}}"
        @?= Just
          ( JObject
              [ ( "foo",
                  JObject
                    [ ("bar", JArray [JNumber 1, JNumber 2, JNumber 3])
                    ]
                )
              ]
          ),
    testCase "invalid input" $ parseJValue "foo" @?= Nothing,
    testCase "weird spacing" $
      parseJValue "   {\t  \n\"foo\"\r \t: \"bar\"\n }  \t"
        @?= Just (JObject [("foo", JString "bar")]),
    testCase "number followed by letter" $ parseJValue "123a" @?= Nothing
  ]

parseJValue :: String -> Maybe JValue
parseJValue s =
  case runStringParser (json <* eof) s of
    Parsed v (StringLines "" _) _ -> Just v
    _ -> Nothing

lexeme :: StringParser a -> StringParser a
lexeme p = spaces *> p <* spaces
  where
    spaces = void $ many $ oneOf " \n\r\t"

symbol :: String -> StringParser String
symbol = lexeme . string

json :: StringParser JValue
json =
  JString <$> stringLiteral
    <|> JNumber <$> number
    <|> JBool <$> bool
    <|> JNull <$ symbol "null"
    <|> JObject <$> object
    <|> JArray <$> array

stringLiteral :: StringParser String
stringLiteral = lexeme $ like '"' *> many (unlike '\"') <* like '"'

number :: StringParser Int
number =
  lexeme
    ( read <$> ((:) <$> like '-' <*> many1 digit)
        <|> read <$> (optional (like '+') *> many1 digit)
    )
    <* notFollowedBy (match isAlpha)
  where
    digit = match isDigit

bool :: StringParser Bool
bool = True <$ symbol "true" <|> False <$ symbol "false"

object :: StringParser [(String, JValue)]
object =
  symbol "{"
    *> sepBy ((,) <$> (stringLiteral <* symbol ":") <*> json) (symbol ",")
    <* symbol "}"

array :: StringParser [JValue]
array = symbol "[" *> sepBy json (symbol ",") <* symbol "]"
