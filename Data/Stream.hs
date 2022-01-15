{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Stream
  ( Stream (..),
  )
where

import Data.Kind (Type)

class Ord (Pos s) => Stream s where
  type Item s :: Type
  type Pos s :: Type

  next :: s -> Maybe (Item s, s)

  getPos :: s -> Pos s
