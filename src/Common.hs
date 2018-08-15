{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Common where

import Algebra.Affine
import qualified Data.List as List
import Data.Monoid (Sum (..))
import Numeric (showHex)

import Util.Vec

type Word logW = BitVector (2^logW)

newtype RegNum = RegNum (Unsigned 5) deriving (Eq, Num, Enum, NFData, Undefined)
instance BitPack RegNum where type BitSize RegNum = 5; pack (RegNum n) = pack n; unpack = RegNum . unpack
instance Show RegNum where show (RegNum n) = List.reverse . List.take 2 $ List.reverse (show n) List.++ List.repeat '0'
instance ShowX RegNum where showsPrecX n = maybe ('X':) (showsPrec n) . maybeX

newtype Ptr logW = Ptr { unPtr :: BitVector (2^logW) }
  deriving (Eq, Enum, NFData, Undefined)
instance BitPack (Ptr logW) where type BitSize (Ptr logW) = 2^logW; pack (Ptr ptr) = ptr; unpack = Ptr
instance KnownNat logW => Show (Ptr logW) where showsPrec _ = showHex . unPtr
instance KnownNat logW => ShowX (Ptr logW) where showsPrecX n = maybe ('X':) (showsPrec n) . maybeX

instance KnownNat n => Group (Sum (BitVector n)) where
    invert = negate

instance KnownNat logW => Affine (Ptr logW) where
    type Diff (Ptr logW) = Sum (Word logW)
    Ptr a `diff` Ptr b = Sum (a - b)
    Sum δ `offset` Ptr a = Ptr (a + δ)

type CodePtr = Ptr
pattern CodePtr :: Word logW -> CodePtr logW
pattern CodePtr {unCodePtr} = Ptr unCodePtr
{-# COMPLETE CodePtr #-}

signed :: KnownNat n => BitVector n -> Signed n
signed = unpack

sliceWordBytes :: ∀ logW logB logA . _ => Ptr logW -> Vec (2^(logW-logB)) (Word logB) -> Word logA
sliceWordBytes (Ptr ptr) bs = chunksLilEndianI @(2^logB) @(2^(logA-logB)) bs !! k
  where (_, k, _) = unpack ptr :: (BitVector _, BitVector (logW-logA), BitVector (logA-logB))
