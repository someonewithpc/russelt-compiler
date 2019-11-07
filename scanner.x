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
    \(                                  { \p s -> TokenLB p }
    \)                                  { \p s -> TokenRB p }
    \{                                  { \p s -> TokenLC p }
    \}                                  { \p s -> TokenRC p }
    \;                                  { \p s -> TokenSep p }
    -- Types and Variables
    \-?$digit+				{ \p s -> TokenInt p (read s :: Int) }
    true                                { \p _ -> TokenBool p True }
    false                               { \p _ -> TokenBool p False }
    let                                 { \p _ -> TokenLet p }
    -- Control flow
    if                                  { \p _ -> TokenIf p }
    else                                { \p _ -> TokenElse p }
    while                               { \p _ -> TokenWhile p }
    $alpha [$alpha $digit \_ !]*        { \p s -> TokenIdentifier p s }

{
data ValueType = VTInt Int
               | VTBool Bool
               | VTAuto String
               deriving Show
data Token =
           -- Types and Variables
           TokenInt AlexPosn Int
           | TokenBool AlexPosn Bool
           | TokenIdentifier AlexPosn String
           -- Attributions
           | TokenLet AlexPosn -- let
           | TokenAtr AlexPosn -- =
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
           -- Control flow
           | TokenIf AlexPosn -- if
           | TokenElse AlexPosn -- else
           | TokenWhile AlexPosn -- while
           deriving (Show)

token_pos (TokenFn p) = p
token_pos (TokenMain p) = p
token_pos (TokenAtr p) = p
token_pos (TokenOp p _) = p
token_pos (TokenLB p) = p
token_pos (TokenRB p) = p
token_pos (TokenLC p) = p
token_pos (TokenRC p) = p
token_pos (TokenSep p) = p
token_pos (TokenInt p _) = p
token_pos (TokenBool p _) = p
token_pos (TokenLet p) = p
token_pos (TokenIdentifier p s) = p
token_pos (TokenIf p) = p
token_pos (TokenElse p) = p
token_pos (TokenWhile p) = p


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
-- token_pos (TokenSep p) = p

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
