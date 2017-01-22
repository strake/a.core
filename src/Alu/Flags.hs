{-# LANGUAGE PatternSynonyms #-}

module Alu.Flags where

import Alu.Impl (Flag (..))

pattern ArithmeticShift, NegateY :: Flag
pattern ArithmeticShift = Flag 1
pattern NegateY = Flag 1
