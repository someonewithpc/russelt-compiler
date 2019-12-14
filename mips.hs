
module MIPS where

import Parser
import InterRep
import Compiler
import Data.Maybe
import Data.Map.Strict as Map ()
import Data.Bifunctor

data MIPS = Nop

instance ASM MIPS where
  compile state ir@(inst, reg, label) = undefined
