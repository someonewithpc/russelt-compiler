{
module Lexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
    [\ \t\f\v\r\n]+			;
    -- Expressions
    [\+\-\*\/\^\%\!\|\&\=\<\>]+         { \p s -> TokenOp p s }
    -- Miscelaneous
    \(                                  { \p s -> TokenLB p }
    \)                                  { \p s -> TokenRB p }
    \{                                  { \p s -> TokenLC p }
    \}                                  { \p s -> TokenRC p }
    -- Types and Variables
    \-?$digit+				{ \p s -> TokenInt p (read s :: Int) }
    true                                { \p _ -> TokenBool p True }
    false                               { \p _ -> TokenBool p False }
    -- Functions
    fn                                  { \p _ -> TokenFn p }
    main                                { \p _ -> TokenMain p }

{
data ValueType = VTInt Int
               | VTBool Bool
               deriving Show
data Token =
           -- Types and Variables
           TokenInt AlexPosn Int
           | TokenBool AlexPosn Bool
           | TokenLet AlexPosn
           -- Expressions
           | TokenOp AlexPosn String
           -- Miscelaneous
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
token_pos (TokenBool p _) = p
token_pos (TokenLet p) = p
--             -- Arithmetic Expressions
token_pos (TokenOp p _) = p
token_pos (TokenLB p) = p
token_pos (TokenRB p) = p
token_pos (TokenLC p) = p
token_pos (TokenRC p) = p
--             -- Attributions
--token_pos (TokenAtr p) = p
--             -- Ifs
--token_pos (TokenIf p) = p
--token_pos (TokenElseIf p) = p
--token_pos (TokenElse p) = p
--             -- While
--token_pos (TokenWhile p) = p
--             -- Functions
--token_pos (TokenFn p) = p
--             -- Miscelaneous
token_pos (TokenSep p) = p

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum

scan_tokens :: String -> [Token]
scan_tokens str = go (alexStartPos, '\n', [], str)
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
