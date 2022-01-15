{-# LANGUAGE TypeFamilies #-}

module Json (jsonTests) where

import Control.Monad (void)
import Control.Monad.Parser
import Data.Char (isAlpha, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
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
  testsWith parseString
    ++ testsWith (parseText . T.pack)

testsWith :: (String -> Maybe JValue) -> [TestTree]
testsWith p =
  [ testCase "string" $ p "\"foo\"" @?= Just (JString "foo"),
    testCase "empty string" $ p "\"\"" @?= Just (JString ""),
    testCase "number" $ p "123" @?= Just (JNumber 123),
    testCase "negative number" $ p "-123" @?= Just (JNumber (-123)),
    testCase "positive number" $ p "+123" @?= Just (JNumber 123),
    testCase "bool true" $ p "true" @?= Just (JBool True),
    testCase "bool false" $ p "false" @?= Just (JBool False),
    testCase "null" $ p "null" @?= Just JNull,
    testCase "object" $
      p "{\"foo\": \"bar\"}"
        @?= Just (JObject [("foo", JString "bar")]),
    testCase "empty object" $ p "{}" @?= Just (JObject []),
    testCase "array" $
      p "[1, 2, 3]"
        @?= Just (JArray [JNumber 1, JNumber 2, JNumber 3]),
    testCase "empty array" $ p "[]" @?= Just (JArray []),
    testCase "nested array" $
      p "[1, [2, 3], 4]"
        @?= Just (JArray [JNumber 1, JArray [JNumber 2, JNumber 3], JNumber 4]),
    testCase "nested object" $
      p "{\"foo\": {\"bar\": \"baz\"}}"
        @?= Just (JObject [("foo", JObject [("bar", JString "baz")])]),
    testCase "nested object with array" $
      p "{\"foo\": {\"bar\": [1, 2, 3]}}"
        @?= Just
          ( JObject
              [ ( "foo",
                  JObject
                    [ ("bar", JArray [JNumber 1, JNumber 2, JNumber 3])
                    ]
                )
              ]
          ),
    testCase "invalid input" $ p "foo" @?= Nothing,
    testCase "weird spacing" $
      p "   {\t  \n\"foo\"\r \t: \"bar\"\n }  \t"
        @?= Just (JObject [("foo", JString "bar")]),
    testCase "number followed by letter" $ p "123a" @?= Nothing
  ]

parseString :: String -> Maybe JValue
parseString s =
  case runStringParser (json <* eof) s of
    Parsed v _ _ -> Just v
    _ -> Nothing

parseText :: Text -> Maybe JValue
parseText t =
  case runTextParser (json <* eof) t of
    Parsed v _ _ -> Just v
    _ -> Nothing

lexeme :: CharParser p => p a -> p a
lexeme p = spaces *> p <* spaces
  where
    spaces = void $ many $ oneOf " \n\r\t"

symbol :: CharParser p => String -> p String
symbol = lexeme . string

json :: CharParser p => p JValue
json =
  JString <$> stringLiteral
    <|> JNumber <$> number
    <|> JBool <$> bool
    <|> JNull <$ symbol "null"
    <|> JObject <$> object
    <|> JArray <$> array

stringLiteral :: CharParser p => p String
stringLiteral = lexeme $ like '"' *> many (unlike '\"') <* like '"'

number :: CharParser p => p Int
number =
  lexeme
    ( read <$> ((:) <$> like '-' <*> many1 digit)
        <|> read <$> (optional (like '+') *> many1 digit)
    )
    <* notFollowedBy (match isAlpha)
  where
    digit = match isDigit

bool :: CharParser p => p Bool
bool = True <$ symbol "true" <|> False <$ symbol "false"

object :: CharParser p => p [(String, JValue)]
object =
  symbol "{"
    *> sepBy ((,) <$> (stringLiteral <* symbol ":") <*> json) (symbol ",")
    <* symbol "}"

array :: CharParser p => p [JValue]
array = symbol "[" *> sepBy json (symbol ",") <* symbol "]"
