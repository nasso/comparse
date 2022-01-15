{-# LANGUAGE TypeFamilies #-}

module Data.Stream.TextLines
  ( TextPos (..),
    TextLines (..),
    fromText,
  )
where

import Data.Stream (Stream (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data TextPos = TextPos Int Int (Vector Text)

instance Show TextPos where
  show (TextPos l c ls) =
    let (lnum, cnum) = (show (l + 1), show (c + 1))
        gut = replicate (length lnum) ' ' ++ " | "
        ngut = lnum ++ " | "
        line = show $ ls ! l
        snippet = gut ++ "\n" ++ ngut ++ line ++ "\n" ++ gut ++ cursor
        cursor = replicate c ' ' ++ "^"
     in lnum ++ ":" ++ cnum ++ ":\n" ++ snippet

instance Eq TextPos where
  (TextPos l1 c1 _) == (TextPos l2 c2 _) = l1 == l2 && c1 == c2

instance Ord TextPos where
  compare (TextPos l1 c1 _) (TextPos l2 c2 _) =
    case compare l1 l2 of
      EQ -> compare c1 c2
      x -> x

start :: Text -> TextPos
start s = TextPos 0 0 $ V.fromList $ T.lines s

data TextLines = TextLines Text TextPos deriving (Eq, Show)

instance Stream TextLines where
  type Item TextLines = Char
  type Pos TextLines = TextPos

  next (TextLines t (TextPos l c ls)) =
    do
      (nextChar, t') <- T.uncons t
      let (l', c') =
            if nextChar == '\n'
              then (l + 1, 0)
              else (l, c + 1)
      pure (nextChar, TextLines t' (TextPos l' c' ls))

  getPos (TextLines _ p) = p

fromText :: Text -> TextLines
fromText s = TextLines s (start s)
