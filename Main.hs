{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CLaSH.Prelude.Safe hiding (Word, toList)
import CLaSH.Prelude.Explicit.Safe
import Data.Foldable (toList)

import Common
import Instruction

import qualified Examples

regFile' :: (KnownNat m, Eq addr, Num addr, Enum addr, Num a) =>
            SClock clk -> SNat n -> Signal' clk (Vec m addr) ->
            Signal' clk (addr, a) -> Signal' clk (Vec m a)
regFile' clk n (unbundle -> srSignals) wbSignal =
    bundle $ (liftA2 (\ case 0 -> pure 0; _ -> id;) <*>
              flip (asyncRam' clk clk n) wbMaySignal) <$> srSignals
  where wbMaySignal = (\ case (0, _) -> Nothing; (n, x) -> Just (n, x);) <$> wbSignal

proc' :: (KnownNat logW, 5 <= logW, 12 <= 2^logW, 32 <= 2^logW) =>
         SClock clk -> (CodePtr logW -> Instruction) ->
         Signal' clk (CodePtr logW, (RegNum, Word logW), Vec 2 RegNum)
proc' clk fetch = o
  where o@(unbundle -> (_, wb, sr)) =
            mealy' clk (\ pc xs ->
                        let i = fetch pc
                            (pc', wb) = case instruct i of
                                Nothing -> error "Illegal instruction"
                                Just op -> op (xs !! (0 :: Int)) (xs !! (1 :: Int)) pc
                        in (pc', (pc, wb, srcRegNums i))) 0 $
            regFile' clk (SNat :: SNat 32) sr wb

type LogWordSize = 6

main :: IO ()
main = mapM_ (\ (pc@(CodePtr z), (n, x), sr) -> do
                  _ <- getLine
                  print pc
                  print ((testProgram . fst .
                           (split :: Word LogWordSize -> (_, BitVector 2))) z)
                  print (n, x)
                  print sr) . toList $ topEntity

topEntity :: Signal (CodePtr LogWordSize, (RegNum, Word LogWordSize), Vec 2 RegNum)
topEntity = proc' systemClock $
            testProgram . fst . (split :: Word LogWordSize -> (_, BitVector 2)) . unCodePtr

testProgram :: Enum addr => addr -> Instruction
testProgram = asyncRom Examples.counter
