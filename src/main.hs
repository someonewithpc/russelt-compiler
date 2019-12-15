module Main where

import Scanner
import Parser
import qualified IR
import Compiler
import MIPS

import Options.Applicative
import Data.Monoid ((<>))
import Data.Maybe
import Data.Map.Strict as Map (singleton, insert, insertLookupWithKey, updateLookupWithKey, (!), Map)
import Control.Monad
import Data.Bifunctor

-- Argument parsing

data Flag = Flag
  {
    input_file :: String,
    output_file :: Maybe String,
    print_token_list :: Bool,
    print_parse_tree :: Bool,
    print_instruction_list :: Bool,
    print_mips :: Bool
  }
  deriving Show

argument_parser :: Parser Flag
argument_parser = Flag
  <$>  argument str
  -- https://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html#v:str
  ( metavar "<file>"
    <> help "Input file" )
  <*> (optional $ strOption
  ( metavar "<file>"
    <> long "output-file"
    <> short 'o'
    <> help "Output file" ))
  <*> switch
  ( long "print-token-list"
    <> short 't'
    <> help "Print the scanner's token list" )
  <*> switch
  ( long "print-parse-tree"
    <> short 'p'
    <> help "Print the parser's tree" )
  <*> switch
  ( long "print-instruction-list"
    <> short 'i'
    <> help "Print the compiler's intermediate code" )
  <*> switch
  ( long "print-mips"
    <> short 'm'
    <> help "Print the compiler's MIPS code" )

splash_screen :: String
splash_screen = "Russel! - A compiler for a subset of Rust to MIPS written in Haskell\n \
                \ |- Version 0.1.0\n \
                \ |- By Diogo Cordeiro and Hugo Sales (DCC-FCUP)\n\n"

main :: IO ()
main = do
  -- Input --
  args <- execParser (info (helper <*> argument_parser) (fullDesc <> header splash_screen))
  raw_input <- readFile $ input_file args

  -- Process --
  let token_list                        = scan_tokens raw_input
  let parse_tree                        = parse token_list
  let (state, ir_blk@(ir_result, _, _)) = IR.ir state parse_tree
  let asm                               = compile state ir_blk :: [MIPS]

  -- Output --
  if (print_token_list args) then
    putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
  else return ()

  if (print_parse_tree args) then
    putStrLn ("Parse Tree:\n" ++ (printTree parse_tree) ++ "\n")
  else return ()

  let compile_output = (unlines $ map show ir_result)
  if (print_instruction_list args) then
    putStrLn ("Instruction List:\n" ++ compile_output)
  else return ()

  if (print_mips args) then
    putStrLn ("Compile Result:\n")
  else return ()

  let out_file = fromMaybe "out.asm" (output_file args)

  writeFile out_file compile_output
