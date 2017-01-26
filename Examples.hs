{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Examples (counter) where

import CLaSH.Prelude.Safe
import Control.Monad (join)
import Language.Haskell.TH
import System.Directory

import Util.Vec

import EmbedFile

$((runIO . listDirectory) "examples" >>=
  (\ f -> fmap join . traverse f)
  (\ path ->
   (:) (SigD (mkName path) $
        ConT ''Vec `AppT` WildCardT `AppT` (ConT ''BitVector `AppT` LitT (NumTyLit 32))) .
   pure . flip (ValD (VarP (mkName path))) [] . NormalB . AppE (VarE 'chunksLilEndianI) <$>
   embedFile ("examples/" <|> path)) . filter (not . any (== '.')))
