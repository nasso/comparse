module Control.Monad.Parser
  ( module Control.Monad.Parser.Class,
    ParserT (..),
    ParseResult (..),
    ParseError (..),
    ErrorDesc (..),
    Parser,
    StringParser,
    runParser,
    runStringParser,
  )
where

import Control.Monad.Identity
import Control.Monad.Parser.Class
import Control.Monad.Trans.Parser
import Data.Stream.StringLines (StringLines)
import qualified Data.Stream.StringLines as StringLines

type Parser s a = ParserT s Identity a

runParser :: Parser s a -> s -> ParseResult a s
runParser p = runIdentity . runParserT p

type StringParser a = Parser StringLines a

runStringParser :: StringParser a -> String -> ParseResult a StringLines
runStringParser p s = runIdentity $ runParserT p $ StringLines.fromString s
