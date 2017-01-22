{-# LANGUAGE PatternSynonyms #-}

module Branch where

import Prelude hiding (Ordering (..))

newtype Cmp = Cmp (BitVector 3) deriving (Eq, NFData, Undefined)
instance BitPack Cmp where type BitSize Cmp = 3; pack (Cmp c) = c; unpack = Cmp
instance Show Cmp where
    show = \ case EQ -> "EQ"; NE -> "NE"; LT -> "LT"; GE -> "GE"; LTU -> "LTU"; GEU -> "GEU"
                  TRUE -> "TRUE"; FALSE -> "FALSE"
instance ShowX Cmp where showsPrecX n = maybe ('X':) (showsPrec n) . maybeX

-- Branch comparator ops
pattern EQ, NE, LT, GE, LTU, GEU, TRUE, FALSE :: Cmp
pattern EQ  = Cmp 0
pattern NE  = Cmp 1
pattern LT  = Cmp 4
pattern GE  = Cmp 5
pattern LTU = Cmp 6
pattern GEU = Cmp 7

-- Not actual opcodes, used for control
pattern TRUE = Cmp 2
pattern FALSE = Cmp 3

{-# COMPLETE EQ, NE, LT, GE, LTU, GEU, TRUE, FALSE #-}
