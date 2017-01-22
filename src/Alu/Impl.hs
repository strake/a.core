module Alu.Impl where

newtype Op = Op (BitVector 3) deriving (Eq, NFData, Undefined)
newtype Flag = Flag (BitVector 1) deriving (Eq, Bits, Num, Show, ShowX, NFData, Undefined)
