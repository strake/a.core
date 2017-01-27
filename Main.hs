{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CLaSH.Prelude.Safe hiding (Word, toList)
import CLaSH.Prelude.Explicit.Safe
import Control.Arrow
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import Common
import Instruction
import Util.Vec

import qualified Examples

regFile' :: (KnownNat m, Enum addr) =>
            SClock clk -> SNat n -> Signal' clk (Vec m addr) ->
            Signal' clk (addr, a) -> Signal' clk (Vec m a)
regFile' clk n (unbundle -> srSignals) wbSignal =
    bundle $ flip (asyncRam' clk clk n) (Just <$> wbSignal) <$> srSignals

proc' :: ∀ clk logW n .
         (KnownNat n, KnownNat logW, logW <= 2^logW, 3 <= logW, 5 <= logW, 12 <= 2^logW, 32 <= 2^logW) =>
         SClock clk -> Vec n (Word logW) ->
         Signal' clk (CodePtr logW, Instruction, (RegNum, Word logW), Vec 2 RegNum,
                      Either RegNum (MemOp logW))
proc' clk boot = bundle (pc, i, wb, srcRegNums <$> i, rdOrMemOp)
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
          where f :: CodePtr logW -> (WordPtr logW, BitVector (logW - 5))
                f = unCodePtr >>> split >>>
                    id *** (split >>> \ case (w, 0 :: BitVector 2) -> w
                                             _ -> error "Misaligned instruction fetch")

                jumpHere :: Instruction
                jumpHere = 0x0000006F

        xs :: Signal' clk (Vec 2 (Word logW))
        xs = (liftA2 . liftA2 . liftA2 . liftA2)
             fromMaybe (regFile' clk (SNat :: SNat 32)) bypass (srcRegNums <$> i) wb
          where bypass = liftA2 . traverse . flip $ \ (rd, z) -> \ case
                             0 -> Just 0
                             rs -> bool Nothing (Just z) (rd == rs)

        z :: Signal' clk (Word logW)
        (pc', (rdOrMemOp, z)) =
            unbundle >>> id *** unbundle $
            (\ case Nothing -> error "Illegal instruction"
                    Just op -> \ (x:>y:>_) -> op x y) . instruct <$> i <*> xs <*> pc

        memRd = blockRam' clk boot zW memWrMay

        (memWrMay, wb') =
            unbundle $
            liftA3 (\ z zW -> \ case
                        Left rd -> (Nothing, (rd, Just z))
                        Right (Load rd) -> (Nothing, (rd, Nothing))
                        Right (Store x) -> (Just (zW, x), (0, Just undefined))) z zW rdOrMemOp

        zW :: Signal' clk (WordPtr logW)
        zW = (\ case (zW, 0) -> zW; _ -> error "Misaligned memory use";) . split <$> z

        wb :: Signal' clk (RegNum, Word logW)
        wb = (\ x -> id *** fromMaybe x) <$> memRd <*> register' clk (0, Just undefined) wb'

type LogWordSize = 6

main :: IO ()
main = mapM_ (\ x -> getLine *> printX x) . toList $ topEntity

topEntity :: Signal (CodePtr LogWordSize, Instruction, (RegNum, Word LogWordSize), Vec 2 RegNum,
                     Either RegNum (MemOp LogWordSize))
topEntity = proc' systemClock (chunksLilEndianI Examples.counter)
