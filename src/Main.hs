{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List as List
import Util.Vec

import Common
import Instruction
import Proc

import EmbedFile

type LogWordSize = 6

main :: IO ()
main = mapM_ (\ (pc@(CodePtr z), (n, x)) -> do
                  _ <- getLine
                  let i = asyncRom testProgram . fst .
                          (split :: Word LogWordSize -> (_, BitVector 2)) $ z
                  putStrLn $ "PC: " List.++ showX pc
                  putStrLn $ "Instruction: " List.++ showX i
                  putStrLn $ "MCode: " List.++ showX (instruct @LogWordSize i)
                  putStrLn $ "Writeback: " List.++ showX (n, x)) . toList $ topEntity

topEntity :: Signal System (CodePtr LogWordSize, (RegNum, Word LogWordSize))
topEntity = go systemClockGen systemResetGen
  where
    go clk rst =
        proc clk rst $
        programRom clk rst testProgram .
        fmap (fst . (split :: Word LogWordSize -> (_, BitVector 2)) . unCodePtr)

programRom :: _ => Clock dom gated -> Reset dom sync -> Vec n Instruction -> Signal dom addr -> Signal dom Instruction
programRom clk rst is pc = register clk rst (pure jumpHere) (pure id) <*> rom clk is pc

testProgram :: Vec _ Instruction
testProgram = chunksLilEndianI $(embedFile "examples/counter")
