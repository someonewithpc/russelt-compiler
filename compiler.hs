
module Compiler where

import Scanner
import Parser
import InterRep

class ASM a where
  compile :: State -> IRBlk -> a
