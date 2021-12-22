module Parser where
import Data.Typeable

-- Data types
data Token = Program | End | Assign | Read | Write | If | While | Greater | Lesser | GrEqual |LsEqual | Equal | Sum | Sub | Mult | Div | Error
    deriving (Eq, Show)

data TokenTree token = Nil | Node2 Token (TokenTree token) (TokenTree token) | Leaf Token | ErrorLeaf String | Init Token String (TokenTree token) |LeafS String | NodeS String (TokenTree token) | Node1 (TokenTree token)
    deriving (Eq, Show)

cop :: String -> Bool --cop: checks if it's a comparison operator
cop y = y `elem` [">", "<", "=>", "=<", "!=", "=="]
eop :: String -> Bool --checks if it's an expression operator
eop y = y `elem` ["+", "-"]
top :: String -> Bool --checks if it's a term operator
top y = y `elem` ["*", "/"]


stat :: [String] -> TokenTree a
stat (t:s) =
        case t of
            "if" -> Node2 If (stat s) Nil
            
            "while" -> Node2 While (stat s) Nil
            
            "read" -> Node2 Read (LeafS (s !! 0)) (stat (tail s)) --need to check if t is an indentifier
            
            "write" -> Node2 Write (LeafS (s !! 0)) (stat (tail s)) --need to check if t is an indentifier
            
            "end" -> Leaf End
            
            _ -> if (isIdent t) then 
                    if (s!!0 == ":=") then Node2 Assign (NodeS t (LeafS (s !! 1 ))) (stat (drop 2 s))-- write expr function
                    else ErrorLeaf "Invalid assign symbol -> ':=' expected"
                else ErrorLeaf "Invalid token"

stat _ = ErrorLeaf "Unexpected end of program" --if it doesnt receive a token when it was supposed to

prog :: [String] -> TokenTree a
prog (h:t) = if (h=="program") then 
                if (t !! 1 /= ";") then ErrorLeaf "';' expected"
                else
                    Init Program (t!!0) (stat (drop 2 t))
            else
                ErrorLeaf "'program' expected"


fact :: String -> [String] -> [String]
fact s1 sn =
            if (s1 !! 0) == '(' then do
                let s1 = (sn !! 0)
                -- E = expr s2 s3
                let s3 = ')'
                let nonparsed = tail sn
                -- e
                (["True"] ++ nonparsed) --temporary
            else
                sn
             
isIdent :: Typeable a => a -> Bool
isIdent a = (typeOf a == typeOf "String")

input = "program caio ; if read book a := b"
parser = prog (words input)

checkToken :: String -> Token a --as we can't do the same we do in Oz for haskell (about the record labels), we have this function to return the right token for each input string
checkToken s = case s of
    ">" -> Greater
    ">=" -> GrEqual
    "<" -> Lesser
    "<=" -> LsEqual
    "==" -> Equal
    "+" -> Sum
    "*" -> Mult
    "/" -> Div
    "-" -> Sub
    _ -> Error

-- beautify :: TokenTree a -> String
-- beautify t = (let name = (getTokenName a))
--             if

-- getTokenName :: TokenTree a -> b
-- getTokenName (TokenTree name _ _) = name
-- getTokenName (TokenTree name _) = name
-- getTokenName (TokenTree name) = name

-- tokenToStr :: Token a -> String
-- tokenToStr a = case a of
--     Program -> "Program"
--     While -> "while"
--     End -> "end"
--     Read -> "read"
--     Write -> "write"
--     Assign -> "Assign"
--     If -> "if"
--     _ -> "?"

-- TO DO
-- Stat 50%
-- Sequence
-- Fact 90%
-- expr, term, comp

-- DONE
-- prog
-- eop
-- cop
-- top

-- ID

