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
      jwb :: JWb }
  deriving (Eq, Show, ShowX, NFData, Undefined, Generic)

data JWb = WbAlu | WbPc | JumpIf Branch.Cmp | JumpAlu | Load | Stor
  deriving (Eq, Show, ShowX, NFData, Undefined, Generic)
