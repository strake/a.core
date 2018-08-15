{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-type-defaults #-}

module Instruction (Instruction, instruct, MCodon (..), jumpHere) where

import Control.Lens

import qualified Alu
import Alu.Ops
import qualified Branch as B
import Common
import MCode
import qualified MCode.Lens as L

type Instruction = BitVector 32

instruct ::
    ∀ logW . _ => Instruction -> Maybe (MCodon logW)
instruct i = guard (0b11 == quad) *> case major of
    OpImm -> set L.imm (Just immI) . regOpOnly <$> aluMay
    Op -> regOpOnly <$> aluMay
    LUI -> Just def { rs1 = 0, imm = Just immU }
    AUIPC -> Just def { imm = Just immU, jwb = WbPc }
    JAL   -> Just def { imm = Just immJ, jwb = JumpIf B.TRUE }
    JALR | 0 <- rs2 -> Just def { imm = Just immI, jwb = JumpAlu }
    BRANCH -> Just def { aluFlags = Alu.NegateY, rd = 0, imm = Just immB,
                         jwb = JumpIf (B.Cmp minor) }
    LOAD -> Just def { imm = Just immI, jwb = Load }
    STOR -> Just def { imm = Just immS, jwb = Stor, rd = 0 }
    _ -> Nothing
  where (_ :: BitVector 7, rs2 :: RegNum, rs1 :: RegNum, minor :: BitVector 3,
         rd :: RegNum, major :: BitVector 5, quad :: BitVector 2) = unpack i

        def :: MCodon logW
        def = MCodon { rd, rs1, rs2, imm = Nothing, jwb = WbAlu, aluOp = ADD, aluFlags = 0 }

        func :: BitVector 7
        func = case (testBit i 5, slice d1 d0 minor) of
            (False, 1) -> let (func', _ :: BitVector logW, _ :: BitVector 20) = unpack i
                          in func' ++# 0
            (False, _) -> 0
            (True,  _) -> slice d31 d25 i

        immI, immS, immB, immU, immJ :: Word logW
        immI = (pack . resize . signed . slice d31 d20) i
        immS = pack ((fst . split) immI :: BitVector (2^logW - 5), slice d11 d7 i)
        immB = pack ((fst . split) immI :: BitVector (2^logW - 11),
                     i ! 7, slice d30 d25 i, slice d11 d8 i)
        immU = pack ((fst . split) immI :: BitVector (2^logW - 12), 0 :: BitVector 12)
        immJ = pack ((fst . split) immI :: BitVector (2^logW - 20),
                     slice d19 d12 i, i ! 20, slice d30 d21 i, 0 :: Bit)

        regOpOnly :: (Alu.Op, Alu.Flag) -> MCodon logW
        regOpOnly (op, flags) = def { aluOp = op, aluFlags = flags }

        aluMay :: Maybe (Alu.Op, Alu.Flag)
        aluMay = (,) op <$> case (op, func) of
            (_,   0x00) -> Just 0
            (ADD, 0x20) -> Just Alu.NegateY
            (SHR, 0x20) -> Just Alu.ArithmeticShift
            _           -> Nothing
          where op = Alu.Op minor

pattern OpImm, Op, OpImm32, Op32, LUI, AUIPC, JAL, JALR, BRANCH, LOAD, STOR, MISC_MEM, SYSTEM :: BitVector 5
pattern OpImm    = 0b00100
pattern Op       = 0b01100
pattern OpImm32  = 0b00110
pattern Op32     = 0b01110
pattern LUI      = 0b01101
pattern AUIPC    = 0b00101
pattern JAL      = 0b11011
pattern JALR     = 0b11001
pattern BRANCH   = 0b11000
pattern LOAD     = 0b00000
pattern STOR     = 0b01000
pattern MISC_MEM = 0b00011
pattern SYSTEM   = 0b11100

-- used for processor initialization
jumpHere :: Instruction
jumpHere = 0x0000006F
