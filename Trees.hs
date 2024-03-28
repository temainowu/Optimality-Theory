module OptimalityTheory.Trees where

data Tree =  Label [Label] Tree Tree | L Label [Label] Tree Tree | X String

data Label = C | T | F | M | I | B | V | K | N | Q | D | G | L Syllable
    deriving (Show)

Syllable = Tree

data PrettyList a = PrettyList [a]

instance Show a => Show (PrettyList a) where
    show (PrettyList []) = ""
    show (PrettyList (x:xs)) = show x ++ '\n' : show (PrettyList xs)

instance Show Tree where
    show (N l t1 t2) = show (PrettyList (toList (N l t1 t2)))

toList :: Tree -> [String]
toList (N l t1 t2) = show l :
                     (("+-" ++ head (toList t1)) 
                     : map ("| " ++) (tail (toList t1))) ++
                     (("+-" ++ head (toList t2)) 
                     : map ("  " ++) (tail (toList t2)))
toList (X s) = [s]

par :: String -> String
par s = '[' : s ++ "]"

showTree :: Tree -> String
showTree (N l t1 t2) = par (show l ++ ' ' : showTree t1 ++ ' ' : showTree t2)
showTree (X s) = par s

