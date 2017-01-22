{-# LANGUAGE UndecidableInstances #-}

module Common where

import Algebra.Affine
import qualified Data.List as List
import Data.Monoid (Sum (..))
import Numeric (showHex)

type Word logW = BitVector (2^logW)

newtype RegNum = RegNum (Unsigned 5) deriving (Eq, Num, Enum, NFData, Undefined)
instance BitPack RegNum where type BitSize RegNum = 5; pack (RegNum n) = pack n; unpack = RegNum . unpack
instance Show RegNum where show (RegNum n) = List.reverse . List.take 2 $ List.reverse (show n) List.++ List.repeat '0'
instance ShowX RegNum where showsPrecX n = maybe ('X':) (showsPrec n) . maybeX

newtype CodePtr logW = CodePtr { unCodePtr :: BitVector (2^logW) }
  deriving (Eq, Enum, NFData, Undefined)
instance KnownNat logW => Show (CodePtr logW) where showsPrec _ = showHex . unCodePtr
instance KnownNat logW => ShowX (CodePtr logW) where showsPrecX n = maybe ('X':) (showsPrec n) . maybeX

instance KnownNat n => Group (Sum (BitVector n)) where
    invert = negate

instance KnownNat logW => Affine (CodePtr logW) where
    type Diff (CodePtr logW) = Sum (Word logW)
    CodePtr a `diff` CodePtr b = Sum (a - b)
    Sum δ `offset` CodePtr a = CodePtr (a + δ)

signed :: KnownNat n => BitVector n -> Signed n
signed = unpack
