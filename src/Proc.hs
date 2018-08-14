{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}

module Proc where

import Algebra.Affine
import Control.Arrow
import Data.Monoid (Sum (..))

import Alu
import Common
import Instruction
import MCode as MC
import qualified Trap
import Util.Ram

proc :: ∀ clk gated sync logW .
        _ =>
        Clock clk gated -> Reset clk sync ->
        (Signal clk (BitVector (2^logW-logW+3)) ->
         Signal clk (BitVector (2^logW-logW+3), Vec (2^(logW-3)) (Maybe (BitVector 8))) ->
         Signal clk (Vec (2^(logW-3)) (BitVector 8))) ->
        Signal clk (Ptr logW, Instruction, (RegNum, Word logW))
proc clk rst memory = bundle (delay clk readAddr, i, wb)
  where
    (readAddr, wb) = mealyB clk rst ctrl' startState (i, xs', pack <$> readBytes)
    readBytes = memory (fst . split . unPtr <$> readAddr) (pure (0, pure Nothing))
    i = register clk rst (pure jumpHere) (sliceWordBytes <$> readAddr) <*> readBytes
    rs2 = unpack . slice d24 d20 <$> i
    rs1 = unpack . slice d19 d15 <$> i
    xs' = bundle $ flip (asyncRam0 clk clk 0) ((unpack . pack *** id) <$> wb) <$> rs1:>rs2:>Nil

    ctrl' state@(State {..}) (i, xs', v)
      | Just rd <- injRd = (state {pc = pc .+ 4, injRd = Nothing}, (pc .+ 4, (rd, v)))
      | Just mc <- instruct i = ctrl state (mc, xs')
      | otherwise =
        (state { pc = mtvec
               , mepc = pc
               , mcause = Trap.IllegalInstruction
               , mtval = resize i
               }, (mtvec, (0, errorX "")))

    ctrl state@(State {pc}) (MCodon {..}, x:>y':>_) =
        (state {pc = pc', injRd = injRd'}, (readAddr, (rd, wbv)))
      where
        y = fromMaybe y' imm
        (carry, z) = alu aluFlags aluOp x y
        wbv :: Word logW
        (pc', wbv) = case jwb of
            WbAlu -> (pc .+ 4, z)
            WbPc target -> (pc .+ 4, unCodePtr pc + target)
            JumpIf target pred ->
                let w = split >>> f *** unpack >>> uncurry (==) $ pred
                    f :: BitVector 2 -> Bit
                    f 0 {- EQ,  NE  -} = reduceOr z
                    f 1 {- ⊤,   ⊥   -} = 0
                    f 2 {- LT,  GE  -} = carry `xor` msb (x `xor` y)
                    f _ {- LTU, GEU -} = carry
                in (pc .+ bool 4 (Sum target) w, unCodePtr (pc .+ 4))
            JumpAlu -> (CodePtr (z .&. complement 1), unCodePtr (pc .+ 4))
            Load -> (pc, errorX "")
        (readAddr, injRd') = case jwb of
            Load -> (Ptr z, Just rd)
            _ -> (pc', Nothing)

startState :: KnownNat logW => State logW
startState = State
  { pc = CodePtr 0, mcause = 0, injRd = Nothing
  , mtvec = CodePtr 0x1000, mscratch = errorX "", mepc = errorX "", mtval = errorX "" }

data State logW = State
  { pc :: CodePtr logW
  , injRd :: Maybe (RegNum)
  , mepc     :: CodePtr logW
  , mtvec    :: CodePtr logW
  , mscratch :: Word logW
  , mcause   :: Word logW
  , mtval    :: Word logW
  } deriving (Undefined, Generic)
