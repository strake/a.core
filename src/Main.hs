{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List as List
import Util.BlockRam

import Common
import Instruction
import Proc

import EmbedFile

type LogWordSize = 6

main :: IO ()
main = mapM_ (\ (pc, i, wb) -> do
                  _ <- getLine
                  putStrLn $ "PC: " List.++ showX pc
                  putStrLn $ "Instruction: " List.++ showX i
                  putStrLn $ "MCode: " List.++ showX (instruct @LogWordSize i)
                  putStrLn $ "Writeback: " List.++ showX wb) . toList $ topEntity

topEntity :: Signal System (CodePtr LogWordSize, Instruction, (RegNum, Word LogWordSize))
topEntity = go systemClockGen systemResetGen
  where
    go clk rst = proc clk rst $ blockRam' clk rst (unconcatI (testProgram ++ pure 0) :: Vec (2^14) _)

testProgram :: Vec _ (BitVector 8)
testProgram = $(embedFile "examples/counter")
