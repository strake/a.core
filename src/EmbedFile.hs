{-# LANGUAGE TemplateHaskellQuotes #-}

module EmbedFile (embedFile) where

import qualified Data.ByteString as BS
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

embedFile :: [Char] -> Q Exp
embedFile path =
    qAddDependentFile path >>
    ParensE . foldr (AppE . AppE (ConE '(:>)) .
                     flip SigE (ConT ''BitVector `AppT` (LitT . NumTyLit) 8) .
                     LitE . IntegerL . fromIntegral) (ConE 'Nil) . BS.unpack <$>
    (runIO . BS.readFile) path
