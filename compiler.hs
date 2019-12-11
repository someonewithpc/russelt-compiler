module Main where
import Scanner
import Parser


compile t = undefined

main :: IO ()
main = do
        raw_input <- getContents
        let token_list = scan_tokens raw_input
        let parse_tree = parse token_list

        putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
        putStrLn ("Parse Tree:\n" ++ (printTree parse_tree))

        compile parse_tree
