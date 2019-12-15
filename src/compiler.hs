
module Compiler where

import Parser
import qualified IR

class ASM a where
  compile :: IR.Vars -> IR.Blk -> [a]
