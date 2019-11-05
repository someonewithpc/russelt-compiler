{
module Lexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
    [\ \t\f\v\r]+			;
    -- Expressions
    [\+\-\*\/\^\%\!\|\&\=\<\>]+         { \p s -> TokenOp p s }
    -- Miscelaneous
    \n                                  { \p s -> TokenNL p }
    -- Types and Variables
    \-?$digit+				{ \p s -> TokenInt p (read s :: Int) }
--    \-?$digit+\.$digit			{ \p s -> TokenFloat p (read s) }

{
data Token =
           -- Types and Variables
           TokenInt AlexPosn Int
--           | TokenFloat AlexPosn Float
           -- Expressions
           | TokenOp AlexPosn String
           -- Types
--           | TokenTInt AlexPosn
--           | TokenTFloat AlexPosn
--           | TokenTBool AlexPosn
           -- Miscelaneous
           | TokenNL AlexPosn -- \n
           | TokenLC AlexPosn -- {
           | TokenRC AlexPosn -- }
           | TokenLB AlexPosn -- (
           | TokenRB AlexPosn -- )
           | TokenSep AlexPosn -- ;
           | TokenFn AlexPosn -- fn
           | TokenMain AlexPosn -- main
           | TokenReturn AlexPosn -- return
             deriving (Show)

-- Extract the position of the token (AlexPosn)
token_pos (TokenInt p _) = p
--token_pos (TokenFloat p _) = p
--token_pos (TokenBool p _) = p
--token_pos (TokenVar p _) = p
--             -- Arithmetic Expressions
token_pos (TokenOp p _) = p
--token_pos (TokenLP p) = p
--token_pos (TokenRP p) = p
token_pos (TokenLB p) = p
token_pos (TokenRB p) = p
token_pos (TokenLC p) = p
token_pos (TokenRC p) = p
--             -- Methods
--token_pos (TokenRead p) = p
--token_pos (TokenPrintln p) = p
--             -- Attributions
--token_pos (TokenAtr p) = p
--             -- Ifs
--token_pos (TokenIf p) = p
--token_pos (TokenElseIf p) = p
--token_pos (TokenElse p) = p
--             -- While
--token_pos (TokenWhile p) = p
--             -- Functions
--token_pos (TokenReturn p) = p
--token_pos (TokenDef p) = p
--             -- Types
--token_pos (TokenTInt p) = p
--token_pos (TokenTFloat p) = p
--token_pos (TokenTBool p) = p
--             -- Miscelaneous
--token_pos (TokenComma p) = p
--token_pos (TokenNew p) = p
--token_pos (TokenEnd p) = p
--token_pos (TokenSep p) = p
token_pos (TokenNL p) = p

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum

trim :: [Token] -> [Token]
trim [] = []
trim ((TokenLC p1):ts) = ts
trim ts = ts

scan_tokens :: String -> [Token]
scan_tokens str = trim (go (alexStartPos, '\n', [], str))
    where go inp@(pos, _, _, str) =
              case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ ->
                        error ("Error at " ++ show (getLineNum(pos)) ++
                               ":" ++ show (getColumnNum(pos)) ++ " near: " ++
                               str)
                AlexSkip inp' len -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
}
