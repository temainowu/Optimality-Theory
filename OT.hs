module OptimalityTheory.OT where
import OptimalityTheory.Phones
import Data.List (sort)

type Comp = Phone -> Phone -> Bool

type Lexeme = [(Phone, [Int])]

type Constraint = Lexeme -> Lexeme -> Int

type Harmony = [Int]

type Grammar = [Constraint]

{- vowel chart:
           P    PV     V  
        -----------------
high    | i•y   ɨ•ʉ   ɯ•u
        |    ɪ•ʏ		•ʊ	
midhigh | e•ø   ɘ•ɵ   ɤ•o
mid     | ɛ•œ	  ə•ɞ   ʌ•ɔ
midlow  | æ•ɶ	  ɐ•    ɑ•ɒ
low     | 		  a•

vowel = "ieɛæɪyøœɶʏɨɘəɐaʉɵɞɯɤʌɑʊuoɔɒ"
-}


-- Auxiliary Functions

-- 1/(backness phone) gives an aproximation of the amount of air trapped in the mouth in the closure of a stop
backness :: Phone -> Int
backness (P _ a p _ _) = fromEnum p + fromEnum a

isVowel :: Manner -> Bool
isVowel m = m `elem` [Vowel High, Vowel MidHigh, Vowel Mid, Vowel MidLow, Vowel Low]

isRounded :: Active -> Bool
isRounded (Tongue Rounded _ _) = True  
isRounded _ = False

isStop :: Manner -> Bool
isStop (Stop _) = True
isStop _ = False

isFricative :: Manner -> Bool
isFricative (Fricative _) = True
isFricative _ = False

sonorityOf :: Phone -> Int
sonorityOf (P _ _ _ m _)
    | m == Click = 0
    | isStop m = 1
    | isFricative m = 2
    | m == Tap = 3
    | m == Trill = 4
    | m == NasalStop = 5
    | m == Approximant = 6
    | m == Vowel High = 7
    | m == Vowel MidHigh = 8
    | m == Vowel Mid = 9
    | m == Vowel MidLow = 10
    | m == Vowel Low = 11

count :: [Bool] -> Int
count [] = 0
count (True:bs) = 1 + count bs
count (False:bs) = count bs

-- toLexeme "xyz" = [(Phone a b c d,[0]),(Phone a b c d,[1]),(Phone a b c d,[2])] 
toLexeme :: String -> Lexeme
toLexeme xs = zip (map charToPhone xs) (map (: []) [0..])

-- inverse of toLexeme
toString :: Lexeme -> String
toString = concatMap (phoneToString . fst)

affix :: String -> String -> String
affix xs ys = xs ++ '+' : ys

groups :: Int -> [a] -> [[a]]
groups n xs = [take n (drop i xs) | i <- [0..length xs - n]]

syllables :: [Phone] -> [[Phone]]
syllables [] = []
syllables (SyllableBoundary:xs) = syllables xs
syllables xs = (\ f (a,as) -> a : f as) syllables (break (== SyllableBoundary) xs)

sizeOfCoda :: [Phone] -> Int
sizeOfCoda [] = 0
sizeOfCoda [x] = 0
sizeOfCoda (x:y:xs)
    | sonorityOf x > sonorityOf y = 1 + sizeOfCoda (y:xs)
    | otherwise = 0 + sizeOfCoda (y:xs)

mostHarmonious :: [Harmony] -> Harmony
mostHarmonious [] = error "empty list"
mostHarmonious [x] = x
mostHarmonious (x:xs)
    | x <= mostHarmonious xs = x
    | otherwise = mostHarmonious xs

optimalForms :: [Harmony] -> [Bool]
optimalForms xs = map (== mostHarmonious xs) xs

mask :: [Bool] -> [a] -> [a]
mask [] _ = []
mask _ [] = []
mask (True:bs) (x:xs) = x : mask bs xs
mask (False:bs) (x:xs) = mask bs xs

eval :: Grammar -> Lexeme -> Lexeme -> Harmony
eval g i o = map (\ f -> f i o) g

-- fix: does not generate all possible forms
gen :: String -> [String]
gen [] = []
gen (x:xs) = map (: xs) (complement [x]) ++ map (x :) (gen xs)

-- IO comparisons:

-- same place
place :: Comp
place (P g0 a0 p0 m0 n0) (P g1 a1 p1 m1 n1) = (p0 == p1) && (a0 == a1)

-- same voicing of obstuents
obsVoice :: Comp
obsVoice (P g0 a0 p0 m0 n0) (P g1 a1 p1 m1 n1) = not (g0 /= g1 && isObstruent m0 && isObstruent m1)

-- nasal agrees in place with following obstruent
nasalObs :: Comp
nasalObs a@(P g0 a0 p0 m0 n0) b@(P g1 a1 p1 m1 n1)
            | m0 == NasalStop && isObstruent m1 && place a b = True
            | not (m0 == NasalStop && isObstruent m1) = True
            | otherwise = False

-- vowel agrees in nasality with following sound
notVN :: Comp
notVN (P g0 a0 p0 m0 n0) (P g1 a1 p1 m1 n1)
            | isVowel m0 && n0 == Nasal && n1 == Nasal = True
            | not (isVowel m0) = True
            | otherwise = False

-- I->O comparisons:

-- nasality preservation
nasal :: Comp
nasal (P g0 a0 p0 m0 n0) (P g1 a1 p1 m1 n1)
    | n0 == Nasal && n1 /= Nasal = False
    | otherwise = True

-- Constraints

-- faithfulness constraints (care about both arguments)

-- no deletion
maxi :: Constraint
maxi i o = count [ or [yp `elem` xps | (x,xps) <- o] | (y,[yp]) <- i]

-- no epenthesis
dep :: Constraint
dep i o = count [ null xps | (x,xps) <- o]

-- feature similarity/preservation, takes {place, obsVoice, nasal} as argument
ident :: Comp -> Constraint
ident f i o = count [ not (f x y) | (x,xps) <- o, (y,[yp]) <- i, yp `elem` xps]
-- "ident nasal" = "IdentI->O(nasal)" in OT
-- "ident place" = "IdentIO(place)" in OT

-- no coalescence
uniformity :: Constraint
uniformity i o = sum [ length xps - 1 | (x,xps) <- o, length xps > 1]

-- markedness constraints (ignore first argument)

-- no nasal vowels
noNasalVowels :: Constraint
noNasalVowels _ o = count [ isVowel m && n == Nasal | (P g a p m n,xps) <- o]

-- no voiced stops
noVoicedStops :: Constraint
noVoicedStops _ o = sum [ backness phone | (phone@(P g a p m n),xps) <- o, m == Stop Tenuis && g == Voiced]

-- no voicless resonants
noVoicelessResonants :: Constraint
noVoicelessResonants _ o = count [ not (isObstruent m) && g == Voiceless | (P g a p m n,_) <- o]

-- no voiced obstruents
noVoicedObstruents :: Constraint
noVoicedObstruents _ o = count [ isObstruent m && g == Voiced | (P g a p m n,_) <- o]

-- adjacent elements must agree in some feature f, takes {place, obsVoice, nasalObs, isṼN, nasal} as argument
agree :: Comp -> Constraint
agree f _ o = length [ 1 | [a,b] <- groups 2 (map fst o), not (f a b)]

-- linear order preservation/no metathesis 
-- usually classed as a faithfulness constraint because indexes are considered part of the input
linearity :: Constraint
linearity _ o = linea (concat [sort i | (_,i) <- o])

linear :: [Int] -> Int
linear xs = sum [x | (x,_) <- bubblesort (zip (repeat 0) xs)]
  where 
    bubblesort'iter :: [(Int,Int)] -> [(Int,Int)]
    bubblesort'iter [] = []
    bubblesort'iter [x] = [x]
    bubblesort'iter ((m,x):(n,y):xs)
      | x > y = (n,y) : bubblesort'iter ((m+1,x):xs)
      | otherwise = (m,x) : bubblesort'iter ((n,y):xs)

    bubblesort' :: [(Int,Int)] -> Int -> [(Int,Int)]
    bubblesort' xs i 
      | i == 0 = xs
      | otherwise = bubblesort' (bubblesort'iter xs) (i - 1) 

    bubblesort :: [(Int,Int)] -> [(Int,Int)]
    bubblesort xs = bubblesort' xs (length xs)

-- no non-back rounded vowels
noFrontRound :: Constraint
noFrontRound _ o = length [ 1 | (P g a p m n,xps) <- o, p /= Velar && isVowel m && isRounded a]

-- syllable constraints:
-- a syllable is too hard to define so a lot of the following constraints are just approximations that will likely never be perfect

-- no complex syllables
noComplex :: Constraint
noComplex _ o = length [ x | x <- groups 3 (map fst o), SyllableBoundary `notElem` x]

-- no coda
noCoda :: Constraint
noCoda _ o = sum (map sizeOfCoda (syllables (map fst o)))

onset :: Constraint
onset _ o = sum (map (f . map sonorityOf) (syllables (map fst o)))
    where
        maxi :: [Int] -> Int
        maxi [x] = x
        maxi (x:xs) = max x (maxi xs)

        f :: [Int] -> Int
        f xs = (fromEnum . null . fst) (break (== (maxi xs)) xs)

-- Main

optimal :: Grammar -> String -> [Lexeme] -> [String]
optimal g i os = map toString (mask (optimalForms (map (eval g (toLexeme i)) os)) os)

friendlyOptimal :: Grammar -> String -> [String] -> [String]
friendlyOptimal g i = optimal g i . map toLexeme

-- better version of optimal if gen ever works:
-- optimal' :: Grammar -> String -> [String]
-- optimal' g i = friendlyOptimal g i (gen i)
-- theoretically should return the most harmonious output form(s) for the given grammar and input form

prop_optimal :: Grammar -> String -> Harmony -> Lexeme -> Bool
prop_optimal g i n o = n <= eval g (toLexeme i) (head (mask (optimalForms [eval g (toLexeme i) o]) [o]))

-- this^ prop is not a test, but uses quickCheck as a substitute for the gen function
-- this should be used thusly:
-- quickCheck (prop_optimal *grammar* *input form* *harmony*)
-- theoretically should return a form of harmony greater than the inputed harmony if one exists
-- so you would need to keep checking smaller and smaller harmonies until you find the smallest one

-- Examples

{-
optimal and friendlyOptimal are the only functions that should be called by the user
they both take a grammar, an input form (which is automatically indexed), and a list of output forms 
(which need be indexed if using optimal, but not if using friendlyOptimal)
and return a list of the most harmonious output forms (which are automatically unindexed)

examples:

example a)
> friendlyOptimal [nasAgr, ident obsVoice] "amda" ["ampa", "amda"]
["ampa"]

nasAgr dominates ident obsVoice, 
so the output form is "ampa" because 'm' agrees with 'p' in place.

tableu:

  amda │ nasAgr │ indent obsVoice │
───────┼────────┼─────────────────┤
  amda │   *!   │                 │
───────┼────────┼─────────────────┤
> ampa │        │        *        │
───────┴────────┴─────────────────┘

example b)
> friendlyOptimal [ident obsVoice, nasAgr] "amda" ["ampa", "amda"]
["amda"]

identIO obsVoice dominates nasAgr,
so the output form is "amda" because 'd' is voiced,
 which matches the voicing of the corresponding input segment 'd'.

tableu:

  amda │ ident obsVoice │ nasAgr │
───────┼────────────────┼────────┤
> amda │                │   *    │
───────┼────────────────┼────────┤
  ampa │       *!       │        │
───────┴────────────────┴────────┘

the forms; toLexeme "amba", toLexeme "anda", toLexeme "embi", [('n',[1]),('d',[2])], and [] 
are all more harmonious forms in both examples,
but they were not chosen because they were not in the list of possible outputs.

-}