{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CLaSH.Prelude.Safe hiding (Word, toList)
import CLaSH.Prelude.Explicit.Safe
import Control.Arrow
import Data.Foldable (toList)

import Common
import Instruction
import Util.Vec

import qualified Examples

regFile' :: (KnownNat m, Eq addr, Num addr, Enum addr, Num a) =>
            SClock clk -> SNat n -> Signal' clk (Vec m addr) ->
            Signal' clk (addr, a) -> Signal' clk (Vec m a)
regFile' clk n (unbundle -> srSignals) wbSignal =
    bundle $ (liftA2 (\ case 0 -> pure 0; _ -> id;) <*>
              flip (asyncRam' clk clk n) wbMaySignal) <$> srSignals
  where wbMaySignal = (\ case (0, _) -> Nothing; (n, x) -> Just (n, x);) <$> wbSignal

proc' :: ∀ clk logW n .
         (KnownNat n, KnownNat logW, logW <= 2^logW, 3 <= logW, 5 <= logW, 12 <= 2^logW, 32 <= 2^logW) =>
         SClock clk -> Vec n (Word logW) ->
         Signal' clk (CodePtr logW, Instruction, (RegNum, Word logW), Vec 2 RegNum)
proc' clk boot = bundle (pc, i, wb, srcRegNums <$> i)
  where pc :: Signal' clk (CodePtr logW)
        pc = register' clk resetVector pc'

        resetVector :: CodePtr logW
        resetVector = 0

        i :: Signal' clk Instruction
        i = -- Block RAM's first output is undefined, so jump to same location
            register' clk (pure jumpHere) (pure id) <*>
            (liftA2 (\ w -> v2bv . (!! w) . reverse . unconcatI . bv2v) (snd . f <$> pc) $
             (resize :: Word logW -> BitVector (2^(logW-5)*32)) <$>
             blockRam' clk boot (fst . f <$> pc') memWrMay)
          where f :: CodePtr logW -> (BitVector (2^logW - (logW - 3)), BitVector (logW - 5))
                f = unCodePtr >>> split >>>
                    id *** (split >>> \ case (w, 0 :: BitVector 2) -> w
                                             _ -> error "Misaligned instruction fetch")

                jumpHere :: Instruction
                jumpHere = 0x0000006F

        xs :: Signal' clk (Vec 2 (Word logW))
        xs = regFile' clk (SNat :: SNat 32) (srcRegNums <$> i) wb

        (pc', wb) =
            unbundle $
            (\ case Nothing -> error "Illegal instruction"
                    Just op -> \ (x:>y:>_) -> op x y) . instruct <$> i <*> xs <*> pc

        memWrMay = pure Nothing

type LogWordSize = 6

main :: IO ()
main = mapM_ (\ x -> getLine *> printX x) . toList $ topEntity

topEntity :: Signal (CodePtr LogWordSize, Instruction, (RegNum, Word LogWordSize), Vec 2 RegNum)
topEntity = proc' systemClock (chunksLilEndianI Examples.counter)
