module Util.BitSet where

(⊆), (⊇), (⊂), (⊃) :: Bits a => a -> a -> Bool
x ⊆ y = zeroBits == x .&. complement y
x ⊂ y = x ⊆ y && x /= y
x ⊇ y = y ⊆ x
x ⊃ y = y ⊂ x
