# comparse

[![Tests](https://github.com/nasso/comparse/actions/workflows/tests.yml/badge.svg)](https://github.com/nasso/comparse/actions/workflows/tests.yml)
[![Hackage](https://img.shields.io/hackage/v/comparse)](https://hackage.haskell.org/package/comparse)

`comparse` is a parser combinator library supporting arbitrary input types.
Combinators do not care about the exact type of the input stream, allowing you
to use them on your own data source, as long as it supports a specific set of
operations. The `ParserT` monad transformer can wrap around another monad,
allowing you to easily implement features such as tracing, context-sensitive
parsing, state machines, and so on. `comparse` does its best to provide
meaningful error messages without sacrificing performances or the developer
experience when writing parsers.

## How?

The main difference between `comparse` and other equivalent libraries is that
the parsers you write are generic over the exact type of the input stream. That
means you can write parsers that work with both `String`, `Text` or any other
source of `Char`s with no modification. The combinators in `comparse` also work
with anything that's an instance of `Stream`, even if it's not a `Stream` of
`Char`s (e.g. a `TokenStream`!).

## Example

The following example shows how to use the `comparse` library to parse a
simplified subset of the [JSON](https://en.wikipedia.org/wiki/JSON) data format:

```hs
-- TypeFamilies is required by the `CharParser` constraint
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (void)
import Control.Monad.Parser
import Data.Char (isAlpha, isDigit)
import Data.Text (Text)
import qualified Data.Text as T

data JValue
  = JString String
  | JNumber Int
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Show)

main :: IO ()
main = do
  input <- getContents
  putStrLn "Parsing as String:"
  print (parseString input)
  putStrLn "Parsing as Text:"
  print (parseText $ T.pack input)

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
```

The `CharParser p` constraint lets us easily write parsers specialised to
character input streams. It's actually defined as a `type` alias:

```hs
type ParserOf i p = (MonadParser p, Item (Input p) ~ i)

type CharParser p = ParserOf Char p
```

This design makes it very easy to write reusable parsers and combinators.

For more examples, please refer to the [tests](test/Parsing.hs).
