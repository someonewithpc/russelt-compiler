{
module Lexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
    -- Functions
    fn                                  { \p _ -> TokenFn p }
    [\ \t\f\v\r\n]+			;
    -- Attributions
    \=                                  { \p _ -> TokenAtr p }
    -- Expressions
    [\+\-\*\/\^\%\!\|\&\=\<\>]+         { \p s -> TokenOp p s }
    -- Miscelaneous
    \(                                  { \p _ -> TokenLB p }
    \)					{ \p _ -> TokenRB p }
    \{					{ \p _ -> TokenLC p }
    \}					{ \p _ -> TokenRC p }
    \;					{ \p _ -> TokenSemi p }
    \,                                  { \p _ -> TokenComma p }
    \:                                  { \p _ -> TokenColon p }
    -- Types and Variables
    \-?$digit+				{ \p s -> TokenInt p (read s :: Int) }
    true                                { \p _ -> TokenBool p True }
    false                               { \p _ -> TokenBool p False }
    let                                 { \p _ -> TokenLet p }
    -- Control flow
    if                                  { \p _ -> TokenIf p }
    else                                { \p _ -> TokenElse p }
    while                               { \p _ -> TokenWhile p }
    read_line                           { \p _ -> TokenReadline p }
    println                             { \p _ -> TokenPrintln p }
    [$alpha \_] [$alpha $digit \_]*     { \p s -> TokenIdentifier p s }

{
data ValueType = VTInt Int
               | VTBool Bool
               | VTAuto String
               deriving Show
data Token =
           -- Types and Variables
           TokenInt AlexPosn Int
           | TokenBool AlexPosn Bool
           -- Attributions
           | TokenLet AlexPosn -- let
           | TokenAtr AlexPosn -- =
           -- Expressions
           | TokenOp AlexPosn String
           -- Miscelaneous
           | TokenIdentifier AlexPosn String
           | TokenLC AlexPosn -- {
           | TokenRC AlexPosn -- }
           | TokenLB AlexPosn -- (
           | TokenRB AlexPosn -- )
           | TokenSemi AlexPosn -- ;
           | TokenColon AlexPosn -- :
           | TokenComma AlexPosn -- ,
           -- Functions
           | TokenFn AlexPosn -- fn
           | TokenReadline AlexPosn -- read_line
           | TokenPrintln AlexPosn -- println
           | TokenMain AlexPosn -- main
           | TokenReturn AlexPosn -- return
           -- Control flow
           | TokenIf AlexPosn -- if
           | TokenElse AlexPosn -- else
           | TokenWhile AlexPosn -- while
           deriving (Show)

token_pos (TokenFn p) = p
token_pos (TokenAtr p) = p
token_pos (TokenOp p _) = p
token_pos (TokenLB p) = p
token_pos (TokenRB p) = p
token_pos (TokenLC p) = p
token_pos (TokenRC p) = p
token_pos (TokenSemi p) = p
token_pos (TokenColon p) = p
token_pos (TokenComma p) = p
token_pos (TokenInt p _) = p
token_pos (TokenBool p _) = p
token_pos (TokenLet p) = p
token_pos (TokenIf p) = p
token_pos (TokenElse p) = p
token_pos (TokenWhile p) = p
token_pos (TokenReadline p) = p
token_pos (TokenPrintln p) = p
token_pos (TokenIdentifier p _) = p

-- -- Extract the position of the token (AlexPosn)
-- token_pos (TokenInt p _) = p
-- token_pos (TokenBool p _) = p
-- token_pos (TokenLet p) = p
--             -- Expressions
-- token_pos (TokenOp p _) = p
-- token_pos (TokenLB p) = p
-- token_pos (TokenRB p) = p
-- token_pos (TokenLC p) = p
-- token_pos (TokenRC p) = p
--             -- Attributions
-- --token_pos (TokenAtr p) = p
--             -- Ifs
-- token_pos (TokenIf p) = p
-- --token_pos (TokenElseIf p) = p
-- token_pos (TokenElse p) = p
--             -- While
-- token_pos (TokenWhile p) = p
--             -- Functions
-- token_pos (TokenFn p) = p
--             -- Miscelaneous
-- token_pos (TokenSemi p) = p

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
                        error ("Error at " ++ show (getLineNum(pos)) ++ ":"
                        ++ show (getColumnNum(pos)) ++ " near: " ++ str)
                AlexSkip inp' len -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
}
