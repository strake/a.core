{-# LANGUAGE PatternSynonyms #-}

module Alu.Ops where

import qualified Data.List as List

import Alu.Impl (Op (..))

pattern ADD, SHL, SLT, SLTU, XOR, SHR, OR, AND :: Op
pattern ADD  = Op 0
pattern SHL  = Op 1
pattern SLT  = Op 2
pattern SLTU = Op 3
pattern XOR  = Op 4
pattern SHR  = Op 5
pattern OR   = Op 6
pattern AND  = Op 7
{-# COMPLETE ADD, SHL, SLT, SLTU, XOR, SHR, OR, AND #-}

instance Show Op where
    show = \ case ADD -> "ADD "; SHL -> "SHL "; SLT -> "SLT "; SLTU -> "SLTU"; XOR -> "XOR ";
                  SHR -> "SHR "; OR  -> " OR "; AND -> "AND "

instance ShowX Op where
    showsPrecX n = maybe ("XXX " List.++) (showsPrec n) . maybeX
