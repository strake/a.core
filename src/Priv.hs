{-# LANGUAGE PatternSynonyms #-}

module Priv where

newtype Priv = Priv (BitVector 2) deriving (Eq, Ord, Enum, Show, ShowX, NFData, Undefined)
instance BitPack Priv where type BitSize Priv = 2; pack (Priv n) = n; unpack = Priv

pattern U, S, H, M :: Priv
pattern U = Priv 0
pattern S = Priv 1
pattern H = Priv 2
pattern M = Priv 3
