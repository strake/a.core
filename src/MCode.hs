{-# LANGUAGE DeriveAnyClass #-}

module MCode where

import qualified Alu
import qualified Branch
import Common

data MCodon logW = MCodon
    { aluOp :: Alu.Op,
      aluFlags :: Alu.Flag,
      rd  :: RegNum,
      rs1 :: RegNum,
      rs2 :: RegNum,
      imm :: Maybe (Word logW),
      jwb :: JWb logW }
  deriving (Eq, Show, ShowX, Undefined, Generic)

data JWb logW = WbAlu | WbPc (Word logW) | JumpIf (Word logW) Branch.Cmp | JumpAlu | Load | Stor
  deriving (Eq, Show, ShowX, NFData, Undefined, Generic)
