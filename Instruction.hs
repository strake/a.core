{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Instruction (Instruction, MemOp (..), instruct, srcRegNums) where

import Data.Bool (bool)
import Data.Function (on)
import CLaSH.Prelude.Safe hiding (Word)
import GHC.Generics

import Common

type Instruction = BitVector 32

srcRegNums :: Instruction -> Vec 2 RegNum
srcRegNums i = RegNum <$> (slice d24 d20 i :> slice d19 d15 i :> Nil)

instruct ::
    ∀ logW .
    (KnownNat logW, 5 <= logW, 12 <= 2^logW, 32 <= 2^logW) =>
    Instruction ->
    Maybe (Word logW -> Word logW -> CodePtr logW ->
           (CodePtr logW, (Either RegNum (MemOp logW), Word logW)))
instruct i = case opcode of
    RegImm -> (\ alu -> regOpOnly $ pure . flip alu immI) <$> aluMay
    RegReg -> regOpOnly <$> aluMay
    RegReg32 -> regOpOnly . (`on` truncAndSExt (SNat :: SNat 5)) <$>
                bool Nothing aluMay (snatToInteger (SNat :: SNat logW) > 5)
    LUI -> Just (regOpOnly $ \ _ _ -> immU)
    AUIPC -> Just (\ _ _ pc@(CodePtr z) -> (pc + 4, (Left rd, z + immU)))
    JAL -> Just (\ _ _ (CodePtr z) -> (CodePtr (z + immUJ), (Left rd, z + 4)))
    JALR | 0 <- rs2 -> Just (\ x _ (CodePtr z) ->
                             (CodePtr (clearBit (x + immI) 0), (Left rd, z + 4)))
    BRANCH -> (\ op x y (CodePtr z) ->
               (CodePtr (z + bool 4 immSB (x `op` y)), (Left 0, undefined))) <$> cmpOpMay
    LOAD | 3 <- funct3 -> Just (\ x _ pc -> (pc + 4, (Right (Load rd), x + immI)))
    STOR | 3 <- funct3 -> Just (\ x y pc -> (pc + 4, (Right (Store y), x + immS)))
    _ -> Nothing
  where opcode = slice d6 d0 i
        funct3 = slice d14 d12 i
        funct7 = slice d31 d25 i

        rs2, rs1, rd :: RegNum
        (rs2 :> rs1 :> rd :> _) =
            RegNum <$> (slice d24 d20 i :> slice d19 d15 i :> slice d11 d7 i :> Nil)

        immI, immS, immSB, immU, immUJ :: Word logW
        immI = ((signExtend :: BitVector 12 -> BitVector ((2^logW - 12) + 12)) .
                slice d31 d20) i
        immS = ((fst . split) immI :: BitVector (2^logW - 5)) ++# slice d11 d7 i
        immSB = ((fst . split) immI :: BitVector (2^logW - 11)) ++#
                (i ! (7 :: BitVector 5)) ++# slice d30 d25 i ++# slice d11 d8 i
        immU = ((fst . split) immI :: BitVector (2^logW - 12)) ++# 0
        immUJ = ((fst . split) immI :: BitVector (2^logW - 20)) ++#
                slice d19 d12 i ++# (i ! (20 :: BitVector 5)) ++# slice d30 d21 i ++# 0

        regOpOnly ::
            (Word logW -> Word logW -> Word logW) ->
            Word logW -> Word logW -> CodePtr logW ->
            (CodePtr logW, (Either RegNum a, Word logW))
        regOpOnly f x y cp = (cp + 4, (Left rd, f x y))

        cmpOpMay :: Maybe (BitVector n -> BitVector n -> Bool)
        cmpOpMay = case funct3 of
            BEQ -> Just (==)
            BNE -> Just (/=)
            BLT -> Just (<)
            BGE -> Just (>=)
            _ -> Nothing

        truncAndSExt :: ∀ logW' . KnownNat logW' => SNat logW' -> Word logW -> Word logW
        truncAndSExt _ = pack . resize . signed . (resize :: Word logW -> Word logW')

        aluMay :: Maybe (Word logW -> Word logW -> Word logW)
        aluMay = case (funct3, funct7') of
            (ADD,  0x00) -> Just (+)
            (ADD,  0x20) -> Just (-)
            (SHL,  0x00) -> Just (\ x -> shiftL x . shamt)
            (SLT,  0x00) -> Just (\ x y -> bool 0 1 $ signed x < signed y)
            (SLTU, 0x00) -> Just (\ x y -> bool 0 1 $ x < y)
            (XOR,  0x00) -> Just xor
            (SHR,  0x00) -> Just (\ x -> shiftR x . shamt)
            (SHR,  0x20) -> Just (\ x -> fromIntegral . shiftR (signed x) . shamt)
            (OR,   0x00) -> Just (.|.)
            (AND,  0x00) -> Just (.&.)
            _ -> Nothing
          where funct7' :: BitVector 7
                funct7' = case (i ! (5 :: BitVector 5), slice d1 d0 funct3) of
                    (0, 1) -> ((fst . split) funct7 :: BitVector (12 - logW)) ++#
                              (0 :: BitVector (logW - 5))
                    (0, _) -> 0
                    (1, _) -> funct7

        shamt :: KnownNat n => BitVector n -> Int
        shamt y = fromIntegral (y .&. (snatToNum . snatProxy) y - 1)

data MemOp logW = Load RegNum | Store (Word logW) deriving (ShowX, Generic)

pattern RegImm = 0x13
pattern RegReg = 0x33
pattern RegReg32 = 0x3B
pattern LUI = 0x37
pattern AUIPC = 0x17
pattern JAL = 0x6F
pattern JALR = 0x67
pattern BRANCH = 0x62
pattern LOAD = 0x03
pattern STOR = 0x23
pattern FENCE = 0x0F
pattern SYSTEM = 0x73

-- ALU ops
pattern ADD  = 0
pattern SHL  = 1
pattern SLT  = 2
pattern SLTU = 3
pattern XOR  = 4
pattern SHR  = 5
pattern OR   = 6
pattern AND  = 7

-- Branch comparator ops
pattern BEQ  = 0
pattern BNE  = 1
pattern BLT  = 4
pattern BGE  = 5
pattern BLTU = 6
pattern BGEU = 7
