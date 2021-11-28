# comparse

[![Tests](https://github.com/nasso/comparse/actions/workflows/tests.yml/badge.svg)](https://github.com/nasso/comparse/actions/workflows/tests.yml)

`comparse` is a parser combinator library supporting arbitrary input types.
Combinators do not care about the exact type of the input stream, allowing you
to use them on your own data source, as long as it supports a specific set of
operations. The `ParserT` monad transformer can wrap around another monad,
allowing you to easily implement features such as tracing, context-sensitive
parsing, state machines, and so on. `comparse` does its best to provide
meaningful error messages without sacrificing performances or the developer
experience when writing parsers.

## Example

The following example shows how to use the `comparse` library to parse a subset
of the [JSON](https://en.wikipedia.org/wiki/JSON) data format:

```hs
import Control.Monad (void)
import Control.Monad.Parser
import Data.Stream.StringLines

data JValue
  = JString String
  | JNumber Int
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Show)

main :: IO ()
main = interact $ show . parseJValue

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
  lexeme $
    read <$> ((:) <$> like '-' <*> many1 digit)
      <|> read <$> (optional (like '+') *> many1 digit)
  where
    digit = oneOf ['0' .. '9']

bool :: StringParser Bool
bool = True <$ symbol "true" <|> False <$ symbol "false"

object :: StringParser [(String, JValue)]
object =
  symbol "{"
    *> sepBy ((,) <$> (stringLiteral <* symbol ":") <*> json) (symbol ",")
    <* symbol "}"

array :: StringParser [JValue]
array = symbol "[" *> sepBy json (symbol ",") <* symbol "]"
```

For more examples, please refer to the [tests](test/Parsing.hs).
