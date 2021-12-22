module Parser where
import Data.Typeable
import Data.Either

-- Data types
data Token = Program | End | Assign | Read | Write | If | While | Greater | Lesser | GrEqual |LsEqual | Equal | Sum | Sub | Mult | Div | Error
    deriving (Eq, Show)

data TokenTree token = Nil | Node2 Token (TokenTree token) (TokenTree token) | Leaf Token | ErrorLeaf String | Init Token String (TokenTree token) |LeafS String | NodeS String (TokenTree token) | Node1 (TokenTree token) | NonParsed String
    deriving (Eq, Show)

cop :: String -> Bool --cop: checks if it's a comparison operator
cop y = y `elem` [">", "<", "=>", "=<", "!=", "=="]
eop :: String -> Bool --checks if it's an expression operator
eop y = y `elem` ["+", "-"]
top :: String -> Bool --checks if it's a term operator
top y = y `elem` ["*", "/"]

-- comp :: String -> [String] ->TokenTree a
-- comp s1 sn = sequen expr cop s1 sn
-- expr :: String -> [String] ->TokenTree a
-- expr s1 sn = sequen term eop s1 sn
-- term :: String -> [String] -> TokenTree a
-- term s1 sn = sequenTerm s1 sn --fact function doesnt return a tokentree, so we need to create another function to it

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

sequen :: (String -> [String] -> TokenTree a) -> (String -> Bool) -> String -> [String] -> TokenTree a
sequen nonterm sep s1 sn = 
                        let x1 = (nonterm s1 sn) in
                            if (isIdent x1) then do
                                let t = s1 !! 0
                                let s3 = tail s1
                                let x2 = sequen nonterm sep s3 sn
                                Node2 (checkToken [t]) x1 x2
                            else
                                NonParsed x1

-- sequenTerm :: String -> [String] -> TokenTree a
-- sequenTerm s1 sn = 

fact :: String -> [String] -> [String]
fact s1 sn =
            if (s1 !! 0) == '(' then do
                let s1 = (sn !! 0)
                -- let e = expr (tail sn) sn
                let s3 = ')'
                let nonparsed = tail sn
                -- [e, nonparsed]
                (["True"] ++ nonparsed) --temporary
                
            else
                sn


isIdent :: Typeable a => a -> Bool
isIdent a = (typeOf a == typeOf "String")

checkToken :: String -> Token --as we can't do the same we do in Oz for haskell (about the record labels), we have this function to return the right token for each input string
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

getNonParsedValue :: TokenTree a -> String -- as "either" function wasn't working, we need to use only one datatype for the functions returnings.
getNonParsedValue (NonParsed value) = value


-- input = "program caio ; if read book a := b"
-- parser = prog (words input)


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

