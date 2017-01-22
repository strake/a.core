{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Alu (Op (..), module Alu.Ops, Flag, module Alu.Flags, alu) where

import Alu.Flags
import Alu.Impl
import Alu.Ops
import Common
import Util.BitSet

alu :: ∀ logW . KnownNat logW => Flag -> Op -> Word logW -> Word logW -> (Bit, Word logW)
alu flags opcode x y' = case opcode of
    ADD  -> (carry, sum)
    SHL  -> (x ! shamt, x `shiftL` shamt)
    SLT  -> (errorX "", (resize . pack) (carry `xor` msb x `xor` msb y))
    SLTU -> (errorX "", (resize . pack) carry)
    XOR  -> (errorX "", x `xor` y)
    SHR  -> (errorX "",
             let f = flip shiftR shamt
             in bool f (pack . f . signed) (ArithmeticShift ⊆ flags) x)
    OR   -> (errorX "", x .|. y)
    AND  -> (errorX "", x .&. y)
  where ifFlag :: Flag -> (a -> a) -> a -> a
        ifFlag flag f = bool id f (flag ⊆ flags)

        y = ifFlag NegateY complement y'

        shamt :: Int
        shamt = fromIntegral (resize y' :: BitVector logW)

        (unpack -> carry, sum) = (split . ifFlag NegateY (+1)) (x `plus` y)
