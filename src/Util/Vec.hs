module Util.Vec where

chunksLilEndianI :: (KnownNat l, KnownNat m, KnownNat n) =>
                    Vec (n*m) (BitVector l) -> Vec n (BitVector (l*m))
chunksLilEndianI = fmap (v2bv . concat . reverse) . unconcatI . fmap bv2v
