{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Parser
  ( module Control.Monad.Parser.Class,
    ParserT (..),
    ParseResult (..),
    ParseError (..),
    ErrorDesc (..),
    Parser,
    StringParser,
    CharParser,
    runParser,
    runStringParser,
    runTextParser,
  )
where

import Control.Monad.Identity
import Control.Monad.Parser.Class
import Control.Monad.Trans.Parser
import Data.Stream (Stream (Item))
import Data.Stream.StringLines (StringLines)
import qualified Data.Stream.StringLines as SL
import Data.Stream.TextLines
import qualified Data.Stream.TextLines as TL
import Data.Text (Text)

type CharParser p = (MonadParser p, Item (Input p) ~ Char)

type Parser s a = ParserT s Identity a

runParser :: Parser s a -> s -> ParseResult a s
runParser p = runIdentity . runParserT p

type StringParser a = Parser StringLines a

runStringParser :: StringParser a -> String -> ParseResult a StringLines
runStringParser p s = runIdentity $ runParserT p $ SL.fromString s

type TextParser a = Parser TextLines a

runTextParser :: TextParser a -> Text -> ParseResult a TextLines
runTextParser p s = runIdentity $ runParserT p $ TL.fromText s
