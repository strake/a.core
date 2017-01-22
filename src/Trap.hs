{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Trap where

import Common
import Priv (Priv (..))

pattern InstructionAddressMisaligned, InstructionAccessFault, IllegalInstruction, Breakpoint,
        LoadAddressMisaligned, LoadAccessFault, StoreAddressMisaligned, StoreAccessFault,
        InstructionPageFault, LoadPageFault, StorePageFault :: KnownNat n => BitVector (n+1)
pattern InstructionAddressMisaligned = Sync 0b0000
pattern InstructionAccessFault       = Sync 0b0001
pattern IllegalInstruction           = Sync 0b0010
pattern Breakpoint                   = Sync 0b0010
pattern LoadAddressMisaligned        = Sync 0b0100
pattern LoadAccessFault              = Sync 0b0101
pattern StoreAddressMisaligned       = Sync 0b0110
pattern StoreAccessFault             = Sync 0b0111
pattern InstructionPageFault         = Sync 0b1100
pattern LoadPageFault                = Sync 0b1101
-- 1110 reserved
pattern StorePageFault               = Sync 0b1111

pattern EnvCall :: ∀ n . (KnownNat n, 3 <= n) => Priv -> BitVector n
pattern EnvCall mode <- (unpack -> (False, 0b10 :: BitVector (n-3), mode)) where
    EnvCall mode = pack (False, 0b10 :: BitVector (n-3), mode)

pattern Interrupt :: ∀ n . (KnownNat n, 5 <= n) => BitVector 2 -> Priv -> BitVector n
pattern Interrupt typ mode <- (unpack -> (True, 0 :: BitVector (n-5), typ, mode)) where
    Interrupt typ mode = pack (True, 0 :: BitVector (n-5), typ, mode)

pattern Sync :: KnownNat n => BitVector n -> BitVector (n+1)
pattern Sync a <- (unpack -> (False, a)) where Sync a = pack (False, a)

pattern Software, Timer, External :: BitVector 2
pattern Software = 0b00
pattern Timer    = 0b01
pattern External = 0b10

data Trap logW = Trap { cause :: Word logW, val :: Word logW, epc :: CodePtr logW }
  deriving (Eq, Show, ShowX, NFData, Undefined, Generic)
