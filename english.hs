data E = A | B | C | D | E
  deriving (Show)

person :: [E]
person = [A, B, C, D, E]

type N = [E]

type V = N -> N -> Bool

type A = E -> Int

-- type I (a -> b -> c) d = (a -> b -> c) -> a -> b -> d

-- type P = a -> b -> Bool

happy :: A
happy A = 1
happy B = 2
happy C = 3
happy D = -2
happy E = 0

evil :: A
evil A = -3
evil B = 3
evil C = -2
evil D = 1
evil E = 2

the :: [a] -> a
the = head

more :: (a -> Int) -> a -> a -> Bool
more a x y = a x >= a y

most :: (a -> Int) -> [a] -> [a]
most a [] = []
most a [x] = [x]
most a (x : xs)
  | more a x (the sortedTail) = x : sortedTail
  | otherwise = the sortedTail : most a (x : drop 1 sortedTail)
  where
    sortedTail = most a xs

most' :: (Show a) => (a -> Int) -> [a] -> IO [a]
most' a e@[] = do
  print e
  return []
most' a [x] = do
  print [x]
  return [x]
most' a (x : xs) = do
  print (x : xs)
  guardClause <- most' a xs
  ( if more a x (the guardClause)
      then
        ( do
            tl <- most' a xs
            return (x : tl)
        )
      else
        ( do
            tl1 <- most' a xs
            tl2 <- most' a xs
            tl3 <- most' a (x : drop 1 tl2)
            return (the tl1 : tl3)
        )
    )

-- is N (how A)
-- if X then Y = Y if X
-- (if) :: Y -> X -> Bool
-- (then) ::
-- `f` a then b = b `f` a