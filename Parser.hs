module Parser where
import Data.Typeable

-- Data types
data Token = Program | End | Assign | Read | Write | If | While | Begin
    deriving (Eq, Show)

data TokenTree token = Nil | Node Token (TokenTree token) | Leaf Token | ErrorLeaf String | Head Token String (TokenTree token)
    deriving (Eq, Show)

cop :: String -> Bool --cop: checks if it's a comparison operator
cop y = y `elem` [">", "<", "=>", "=<", "!=", "=="]
eop :: String -> Bool --checks if it's an expression operator
eop y = y `elem` ["+", "-"]
top :: String -> Bool --checks if it's a term operator
top y = y `elem` ["*", "/"]


stat :: [Token] -> TokenTree a
stat (t:s) =if s == [] then Leaf t else Node t (stat s)
stat _ = ErrorLeaf "Token error"

prog :: [String] -> TokenTree a
prog (h:t) = if (h=="program") then 
                if (t !! 1 /= ";") then ErrorLeaf " ';' expected"
                else
                    Head Program (t!!0) Nil --insert the other functions here
            else
                ErrorLeaf " 'program' expected"


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
             




-- TO DO
-- Stat 10%
-- Sequence
-- Fact 90%
-- ID
-- expr, term, comp

-- DONE
-- prog 80%
-- eop
-- cop
-- top

