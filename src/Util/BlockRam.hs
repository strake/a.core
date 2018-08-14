{-# LANGUAGE ViewPatterns #-}

module Util.BlockRam where

blockRam'
 :: (KnownNat m, KnownNat n, 1 <= n, Eq addr, Enum addr, Undefined a)
 => Clock dom gated
 -> Reset dom sync
 -> Vec n (Vec m a)
 -> Signal dom addr
 -> Signal dom (addr, Vec m (Maybe a))
 -> Signal dom (Vec m a)
blockRam' clk rst a₀ rx (unbundle -> (addr, unbundle -> valu)) =
    bundle $ (\ a₀ valu -> readNew rst clk (blockRam clk a₀) rx (fmap . (,) <$> addr <*> valu)) <$> sequenceA a₀ <*> valu
