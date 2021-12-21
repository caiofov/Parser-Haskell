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


-- stat :: [Token] -> TokenTree a
-- stat (t:s) = Node t (Node (stat s) Nil)

prog :: [String] -> TokenTree a
prog (h:t) = if (h=="program") then 
                if (t !! 1 /= ";") then ErrorLeaf "; expected"
                else
                    Head Program (t!!0) Nil --insert the other functions here
            else
                ErrorLeaf "program expected"

-- t1 = [While, If, Begin]
-- t2 = stat t1

-- isId :: a -> Bool
-- isId a = typeOf a == typeOf "String" || typeOf a == typeOf While


-- TO DO
-- Prog
-- Stat
-- Sequence
-- Fact
-- ID

-- DONE
-- eop
-- cop
-- top

