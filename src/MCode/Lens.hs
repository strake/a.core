{-# LANGUAGE TemplateHaskell #-}

module MCode.Lens where

import MCode
import Control.Lens.TH

$(mkLens (dropWhile (== '_')) ''MCodon)
