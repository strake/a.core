{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common where

import CLaSH.Prelude.Safe hiding (Word)

type Word logW = BitVector (2^logW)

newtype RegNum = RegNum (BitVector 5) deriving (Eq, Num, Enum, Show, ShowX)

newtype CodePtr logW = CodePtr { unCodePtr :: BitVector (2^logW) }
  deriving (Eq, Num, Enum, Show, ShowX)

signed :: KnownNat n => BitVector n -> Signed n
signed = unpack
