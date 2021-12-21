module Parser where
data Token = Program | End | Assign | Read | Write | If | While | Begin
    deriving (Eq, Show)

data TokenTree token = Nil 
                    | Tree token (TokenTree token) 
                    | Leaf token
                    deriving (Eq, Show)
    
--cop: checks if it's a comparison operator
cop :: String -> Bool
cop y = case y of
    ">" -> True
    "<" -> True
    "=>" -> True
    "=<" -> True
    "!=" -> True
    "==" -> True
    _ -> False

eop :: String -> Bool --checks if it's an expression operator
eop y = case y of
    "+" -> True
    "-" -> True
    _ -> False

top :: String -> Bool --checks if it's a term operator
top y = case y of
    "*" -> True
    "/" -> True
    _ -> False

-- stat :: [Token] -> TokenTree t
-- stat (t:s) = case t of
--     [] -> Nil
--     _ -> TokenTree t s

-- t1 = [While, If, Begin]
-- t2 = stat t1