module Prelude (module A) where

import Clash.Explicit.Prelude.Safe as A hiding (Word, fold, foldl, foldl1, foldr, foldr1, toList, length, id, (.))

import Algebra as A (Group (..))
import Control.Category as A (Category (..))
import Control.DeepSeq as A (NFData)
import Control.Monad as A (Monad ((>>=)), (>=>), (<=<), (=<<), guard)
import Data.Bool as A (bool)
import Data.Foldable as A (Foldable (..))
import Data.Maybe as A (fromMaybe)
import GHC.Generics as A (Generic)
