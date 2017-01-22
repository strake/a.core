{-# LANGUAGE TemplateHaskell #-}

module Examples (counter) where

import CLaSH.Prelude.Safe
import Language.Haskell.TH
import System.Directory

import Util.Vec

import EmbedFile

$((runIO . listDirectory) "examples" >>=
  mapM (\ path ->
        flip (ValD (VarP (mkName path))) [] . NormalB . AppE (VarE 'chunksLilEndianI) <$>
        embedFile ("examples/" <|> path)) . filter (not . any (== '.')))
