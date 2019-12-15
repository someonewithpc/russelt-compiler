{
module Scanner where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
    [\ \t\f\v\r\n]+			;
    -- Functions
    fn                                  { \p _ -> TokenFn p }
    -- Expression followed by Attribution
    \+\+|\-\-|[\*\/\%\+\-]\=            { \p s -> TokenOpAtr p s}
    -- Attributions
    \=                                  { \p _ -> TokenAtr p }
    -- Expressions
    \&\&|\|\||[\!\<\>\=]\=|[\*\/\%\!\<\>\+\-]  { \p s -> TokenOp p s }
    -- Miscelaneous
    \(                                  { \p _ -> TokenLB p }
    \)					{ \p _ -> TokenRB p }
    \{					{ \p _ -> TokenLC p }
    \}					{ \p _ -> TokenRC p }
    \;					{ \p _ -> TokenSemi p }
--    \,                                  { \p _ -> TokenComma p }
    \:                                  { \p _ -> TokenColon p }
    -- Types and Variables
    \-?$digit+				{ \p s -> TokenInt p (read s :: Int) }
    true                                { \p _ -> TokenBool p True }
    false                               { \p _ -> TokenBool p False }
    let                                 { \p _ -> TokenLet p }
    bool                                { \p s -> TokenTBool p Tbool }
    \"(\\.|[^\\\"\n])*\"                { \p s -> TokenString p s }
    (i|u)(8|16|32|64|128|size)          { \p s -> TokenTInt p (read ('T' : s) :: Type) }
    -- Control flow
    if                                  { \p _ -> TokenIf p }
    else                                { \p _ -> TokenElse p }
    while                               { \p _ -> TokenWhile p }
    read_line                           { \p _ -> TokenReadline p }
    println\!                           { \p _ -> TokenPrintln p }
    [$alpha \_] [$alpha $digit \_]*     { \p s -> TokenIdentifier p s }

{

data Type = Ti8 | Ti16 | Ti32 | Ti64 | Ti128 | Tisize
          | Tu8 | Tu16 | Tu32 | Tu64 | Tu128 | Tusize
          | Tbool
          | Tstring
          | Tauto
          deriving (Show, Read)

data ValueType = VTInt Int Type
               | VTBool Bool
               | VTString String
               | VTAuto String
               deriving Show

data Token =
           -- Types and Variables
           TokenInt AlexPosn Int                     -- int value
           | TokenBool AlexPosn Bool                 -- bool value
           | TokenString AlexPosn String             -- string value
           | TokenTInt AlexPosn Type                 -- int type
           | TokenTBool AlexPosn Type                -- bool type
           | TokenTString AlexPosn                   -- string type
           | TokenOpAtr AlexPosn String
           -- Attributions
           | TokenLet AlexPosn                       -- let
           | TokenAtr AlexPosn                       -- =
           -- Expressions
           | TokenOp AlexPosn String
           -- Miscelaneous
           | TokenIdentifier AlexPosn String
           | TokenLC AlexPosn                        -- {
           | TokenRC AlexPosn                        -- }
           | TokenLB AlexPosn                        -- (
           | TokenRB AlexPosn                        -- )
           | TokenSemi AlexPosn                      -- ;
           | TokenColon AlexPosn                     -- :
           | TokenComma AlexPosn                     -- ,
           -- Functions
           | TokenFn AlexPosn                        -- fn
           | TokenReadline AlexPosn                  -- read_line
           | TokenPrintln AlexPosn                   -- println
           | TokenMain AlexPosn                      -- main
           | TokenReturn AlexPosn                    -- return
           -- Control flow
           | TokenIf AlexPosn                        -- if
           | TokenElse AlexPosn                      -- else
           | TokenWhile AlexPosn                     -- while
           deriving (Show)

---- Extract the position of the token (AlexPosn)
-- Sadness. There must be a better way
-- token_pos :: Token a => a -> AlexPosn
-- token_pos (_ p) = p
-- token_pos (_ p _) = p

-- Command to generate the section bellow:
-- sed -r -e 's/ *--.*//g' -e 's/.*((Token.*) AlexPosn( \w+)?( \w+)?).*/token_pos (\2 p\3\4) = p/g' -e 's/(.* p) \w+/\1 _/g' -e 's/_ \w+/_ _/g' -e '/^$/d'

token_pos (TokenInt p _) = p
token_pos (TokenBool p _) = p
token_pos (TokenString p _) = p
token_pos (TokenTBool p _) = p
token_pos (TokenTInt p _) = p
token_pos (TokenLet p) = p
token_pos (TokenOpAtr p _) = p
token_pos (TokenAtr p) = p
token_pos (TokenOp p _) = p
token_pos (TokenIdentifier p _) = p
token_pos (TokenLC p) = p
token_pos (TokenRC p) = p
token_pos (TokenLB p) = p
token_pos (TokenRB p) = p
token_pos (TokenSemi p) = p
token_pos (TokenColon p) = p
token_pos (TokenComma p) = p
token_pos (TokenFn p) = p
token_pos (TokenReadline p) = p
token_pos (TokenPrintln p) = p
token_pos (TokenMain p) = p
token_pos (TokenReturn p) = p
token_pos (TokenIf p) = p
token_pos (TokenElse p) = p
token_pos (TokenWhile p) = p

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
