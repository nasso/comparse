{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Parser.Class
  ( module Control.Monad.Parser.Class,
  )
where

import Control.Applicative ((<**>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Stream

infixl 3 <|>

infixl 1 <?>

-- | A monad with parsing capabilities.
class Monad m => MonadParser s m | m -> s where
  -- | The current input stream.
  parseStream :: m s

  -- | Replace the input stream.
  setParseStream :: s -> m ()

  -- | A parser that always fails.
  noParse :: m a

  -- | A parser that returns the next item.
  item :: m (Item s)

  -- | @followedBy p@ is a parser that succeeds if @p@ succeeds, but it does not
  -- consume any input.
  followedBy :: m a -> m ()

  -- | @notFollowedBy p@ is a parser that only succeeds if @p@ fails. This
  -- parser will not consume any input.
  notFollowedBy :: m a -> m ()

  -- | @try p@ is a parser that does everything like @p@, except it forcefully
  -- resets the position of any error reported by @p@ to the current position.
  try :: m a -> m a

  -- | @p <|> q@ is a parser that is equivalent to @p@ when @p@ succeeds and
  -- @q@ when @p@ fails to parse anything.
  (<|>) :: m a -> m a -> m a

  -- | @p <?> msg@ is a parser that behaves like @p@, but when @p@ fails, it
  -- reports an error indicating that @msg@ was the expected input.
  (<?>) :: m a -> String -> m a

-- | Parser that succeeds if the stream is empty. Does not consume any items.
eof :: MonadParser s m => m ()
eof = notFollowedBy item <?> "end of input"

-- | Fail with an "expected" message.
expected :: MonadParser s m => String -> m a
expected s = noParse <?> s

-- | Succeeds only if the value parsed by the parser satisfies the predicate.
satisfy :: MonadParser s m => m a -> (a -> Bool) -> m a
satisfy p f = try $ do
  i <- p
  if f i
    then return i
    else noParse

-- | Parse a single item satisfying the given predicate.
match :: MonadParser s m => (Item s -> Bool) -> m (Item s)
match = satisfy item

-- | Make a parser optional.
optional :: MonadParser s m => m a -> m (Maybe a)
optional p = Just <$> p <|> pure Nothing

-- | Try a series of parsers in order, returning the first one that succeeds.
choice :: MonadParser s m => [m a] -> m a
choice = foldr (<|>) noParse

-- | Try to run the given parser as many times as possible.
many :: MonadParser s m => m a -> m [a]
many p = ((:) <$> p <*> many p) <|> pure []

-- | Try to run the given parser as many times as possible, but at least once.
-- The result is returned as a regular list, but is guaranteed to be non-empty.
many1 :: MonadParser s m => m a -> m [a]
many1 p = (:) <$> p <*> many p

-- | Try to run the given parser as many times as possible, but at least once.
some :: MonadParser s m => m a -> m (NonEmpty a)
some p = (:|) <$> p <*> many p

-- | Parse a non-empty series of @a@ separated by @b@s (without a trailing @b@).
sepBy1 :: MonadParser s m => m a -> m b -> m (NonEmpty a)
sepBy1 a b = (:|) <$> a <*> many (b *> a)

-- | Parse a potentially empty series of @a@ separated by @b@s (without a
-- trailing @b@).
sepBy :: MonadParser s m => m a -> m b -> m [a]
sepBy a b = NonEmpty.toList <$> sepBy1 a b <|> pure []

-- | Parse any value equal to @a@.
like :: (MonadParser s m, Eq (Item s), Show (Item s)) => Item s -> m (Item s)
like a = item `satisfy` (== a) <?> show a

-- | Parse any value not equal to @a@.
unlike :: (MonadParser s m, Eq (Item s), Show (Item s)) => Item s -> m (Item s)
unlike a = item `satisfy` (/= a) <?> "anything but " ++ show a

-- | Parse a continuous sequence of items equal to the given one.
string ::
  (MonadParser s m, Eq (Item s), Show (Item s)) =>
  [Item s] ->
  m [Item s]
string [] = return []
string (x : xs) = like x >> string xs >> return (x : xs)

-- | Parse any value equal to at least one element of the given list.
oneOf :: (MonadParser s m, Eq (Item s), Show (Item s)) => [Item s] -> m (Item s)
oneOf l = item `satisfy` (`elem` l) <?> "one of " ++ show l

-- | Parse any value not equivalent to any element of the given list.
-- For a version that accepts non-Show items, see @noneOf'@.
noneOf ::
  (MonadParser s m, Eq (Item s), Show (Item s)) =>
  [Item s] ->
  m (Item s)
noneOf l = item `satisfy` (`notElem` l) <?> "none of " ++ show l

-- | @chainl1 p op@ Parse a chain of *one* or more occurrences of @p@,
-- separated by @op@. Return a value obtained by a left associative application
-- of all functions returned by @op@ to the values returned by @p@.
--
-- This is particularly useful for parsing left associative infix operators.
chainl1 :: MonadParser s m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

-- | @chainr1 p op@ Parse a chain of *one* or more occurrences of @p@,
-- separated by @op@. Return a value obtained by a right associative application
-- of all functions returned by @op@ to the values returned by @p@.
--
-- This is particularly useful for parsing right associative infix operators.
chainr1 :: MonadParser s m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan
  where
    scan = p <**> rst
    rst = (flip <$> op <*> scan) <|> pure id

-- | Run a parser on a different stream of items.
withInput :: MonadParser s m => s -> m a -> m (a, s)
withInput s' p = do
  s <- parseStream
  setParseStream s'
  x <- p
  s'' <- parseStream
  setParseStream s
  return (x, s'')
