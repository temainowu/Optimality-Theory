type Dic u = [(String, u)]

(!) :: Dic u -> String -> u
d ! x = head [u | (a,u) <- d, a == x]

remove :: String -> Dic u -> Dic u
remove x d = [(a,u) | (a,u) <- d, a /= x]  

insert :: (String, u) -> Dic u -> Dic u
insert (a,u) d = (a,u) : remove a d