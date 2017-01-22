module Util.Vec where

import CLaSH.Prelude.Safe

chunksLilEndianI :: (KnownNat l, KnownNat m, KnownNat n) =>
                    Vec (n*m) (BitVector l) -> Vec n (BitVector (l*m))
chunksLilEndianI = fmap (v2bv . concat . reverse) . unconcatI . fmap bv2v
