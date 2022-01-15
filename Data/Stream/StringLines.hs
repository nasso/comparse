{-# LANGUAGE TypeFamilies #-}

module Data.Stream.StringLines
  ( StringPos (..),
    StringLines (..),
    fromString,
  )
where

import Data.Stream (Stream (..))

data StringPos = StringPos Int Int String

instance Show StringPos where
  show (StringPos l c ls) =
    lnum ++ ":" ++ cnum ++ ":\n" ++ snippet
    where
      lnum = show (l + 1)
      cnum = show (c + 1)
      gut = replicate (length lnum) ' ' ++ " | "
      ngut = lnum ++ " | "
      snippet = gut ++ "\n" ++ ngut ++ ls ++ "\n" ++ gut ++ cursor
      cursor = replicate c ' ' ++ "^"

instance Eq StringPos where
  (StringPos l1 c1 _) == (StringPos l2 c2 _) = l1 == l2 && c1 == c2

instance Ord StringPos where
  compare (StringPos l1 c1 _) (StringPos l2 c2 _) =
    case compare l1 l2 of
      EQ -> compare c1 c2
      x -> x

start :: String -> StringPos
start s = StringPos 0 0 $ takeWhile (/= '\n') s

adv :: StringPos -> StringPos
adv (StringPos l c s) = StringPos l (c + 1) s

nextl :: StringPos -> String -> StringPos
nextl (StringPos l _ _) s = StringPos (l + 1) 0 (takeWhile (/= '\n') s)

data StringLines = StringLines String StringPos deriving (Eq, Show)

instance Stream StringLines where
  type Item StringLines = Char
  type Pos StringLines = StringPos

  next (StringLines [] _) = Nothing
  next (StringLines ('\n' : xs) p) = Just ('\n', StringLines xs (nextl p xs))
  next (StringLines (x : xs) p) = Just (x, StringLines xs (adv p))

  getPos (StringLines _ p) = p

fromString :: String -> StringLines
fromString s = StringLines s (start s)
