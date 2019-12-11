module Main where
import System.Environment
import Data.List
import Scanner
import Parser

data Flag = Help | Output String | PrintTokenList | PrintParseTree | Quiet | File String deriving (Show, Eq, Ord)

check_flag :: Flag -> [Flag] -> Bool
check_flag _ [] = False
check_flag f (p:xs)
  | f == p    = True
  | otherwise = check_flag f xs

help_screen :: String
help_screen = "Usage:\n\n\tcompiler [command-line-options] <input-file>\n\n Commands:\n\n\
           \ \t-h\t\t\tHelp info \n \
           \ \t-o <file>\t\tSet '<file>.asm' as output file (default 'out.asm')\n \
           \ \t--print-token-list\tPrint scanner's token list\n \
           \ \t--print-parse-tree\tPrint parser's tree\n \
           \ \t-q\t\t\tQuiet compile (don't print the header nor the final message)\n"

splash_screen :: String
splash_screen = "Russel! - A compiler for a subset of Rust to MIPS written in Haskell\n \
           \ |- run <compiler -h> for help\n \
           \ |- Version 0.1.0\n \
           \ |- By Diogo Cordeiro and Hugo Sales (DCC-FCUP)\n\n"

from_just :: String -> Maybe b -> b
from_just msg Nothing = error ("ERROR: " ++ msg)
from_just _ (Just x) = x

argument_handler :: [String] -> [Flag]
argument_handler [] = []
argument_handler ("-h":xs) = Help : (argument_handler xs)
argument_handler ("-q":xs) = Quiet : (argument_handler xs)
argument_handler ("--print-token-list":xs) = PrintTokenList : (argument_handler xs)
argument_handler ("--print-parse-tree":xs) = PrintParseTree : (argument_handler xs)
argument_handler ("-o":f:xs) = (Output f) : (argument_handler xs)
argument_handler (f:xs) = (File f) : (argument_handler xs)

get_file_output :: [Flag] -> String
get_file_output [] = "out.asm"
get_file_output (Output s:xs) = s ++ ".asm"
get_file_output (_:xs) = get_file_output xs

get_file_input :: [Flag] -> Maybe String
get_file_input [] = Nothing
get_file_input (File s:xs) = Just s
get_file_input (_:xs) = get_file_input xs

compile t = undefined

main :: IO ()
main = do
  arg_list <- getArgs
  let flag_list = sort (argument_handler(fmap id arg_list))

  let quiet_compile = check_flag Quiet flag_list
  if (not quiet_compile) then
    putStr splash_screen
  else return()

  let print_help_screen = check_flag Help flag_list
  if (print_help_screen) then
    putStr help_screen
  else return()

  let outputFile = get_file_output flag_list
  raw_input <- readFile (from_just "no input files\nTry the '-h' option for basic information" (get_file_input flag_list))

  let token_list = scan_tokens raw_input
  let parse_tree = parse token_list

  let print_token_list = check_flag PrintTokenList flag_list
  if print_token_list then
    putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
  else return()

  let print_parse_tree = check_flag PrintParseTree flag_list
  if print_parse_tree then
    putStrLn ("Parse Tree:\n" ++ (printTree parse_tree) ++ "\n")
  else return()

  compile parse_tree

  if (not quiet_compile) then
    putStrLn "Russel! is done compiling."
  else return()
