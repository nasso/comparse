{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Stream
  ( Stream (..),
  )
where

import Data.Data (Proxy (..))
import Data.Kind (Type)

class Ord (Pos s) => Stream s where
  type Item s :: Type
  type Chunk s :: Type
  type Pos s :: Type

  next :: s -> Maybe (Item s, s)
  nextWhile :: (Item s -> Bool) -> s -> (Chunk s, s)
  nextN :: Int -> s -> (Chunk s, s)

  makeChunk :: Proxy s -> [Item s] -> Chunk s
  unmakeChunk :: Proxy s -> Chunk s -> [Item s]

  getPos :: s -> Pos s
