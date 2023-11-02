module OptimalityTheory.OT where
import OptimalityTheory.Phones
import Test.QuickCheck

type Comp = Phone -> Phone -> Bool

type Lexeme = [(Phone, [Int])]

type Constraint = Lexeme -> Lexeme -> Int

type Harmony = [Int]

type Grammar = [Constraint]

-- Classes of sounds

{- vowel chart:
i		y   ɨ   ɯ		u
	ɪ			    ʊ	
e		ø   ɵ   ɤ		o
ɛ		œ	ə   ʌ		ɔ
æ		ɶ	ɐ   ɑ		ɒ
			a			

this is based on dr geoff lindey's vowel chart 
but with the addition of ɶ, ɨ, and ɒ 
and considering æ to be a cardinal vowel.

vowel = "ieɛæɪyøœɶɵəɐaɯɤʌɑʊuoɔɒ"
lax = "ɪɵʊəɐ"
-}


-- Auxiliary Functions

-- 1/(backness phone) gives an aproximation of the amount of air trapped in the mouth in the closure of a stop
backness :: Phone -> Int
backness (P _ a p _ _) = fromEnum p + fromEnum a

isVowel :: Manner -> Bool
isVowel m = m `elem` [Vowel High, Vowel MidHigh, Vowel Mid, Vowel MidLow, Vowel Low]

isRounded :: Active -> Bool
isRounded (Tongue (Dorsal Rounded) _) = True
isRounded _ = False

sonorityOf :: Phone -> Int
sonorityOf (P _ _ _ m _)
    | m == Click = 0
    | m `elem` [Stop Tenuis, Stop Unreleased, Stop Aspirated] = 1
    | m `elem` [Stop (Fricated Sibilant), Stop (Fricated NonSibilant)] = 2
    | m `elem` [Fricative Sibilant, Fricative NonSibilant] = 3
    | m == Tap = 4
    | m == Trill = 5
    | m == NasalStop = 6
    | m == Approximant = 7
    | m == Vowel High = 8
    | m == Vowel MidHigh = 9
    | m == Vowel Mid = 10
    | m == Vowel MidLow = 11
    | m == Vowel Low = 12

-- \\ is the set difference operator
(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = [x | x <- xs, x `notElem` ys]

-- complement of set
complement :: [Char] -> [Char]
complement xs = universe \\ xs

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
maxi i o = length [ 1 | (y,[yp]) <- i, or [yp `elem` xps | (x,xps) <- o]]

-- no epenthesis
dep :: Constraint
dep i o = length [ 1 | (x,xps) <- o, null xps]

-- feature similarity/preservation, takes {place, obsVoice, nasal} as argument
ident :: Comp -> Constraint
ident f i o = length [ 1 | (x,xps) <- o, (y,[yp]) <- i, not (f x y), yp `elem` xps]
-- "ident nasal" = "IdentI->O(nasal)" in OT
-- "ident place" = "IdentIO(place)" in OT

-- no coalescence
uniformity :: Constraint
uniformity i o = sum [ length xps - 1 | (x,xps) <- o, length xps > 1]

-- markedness constraints (ignore first argument)

-- no nasal vowels
noNasalVowels :: Constraint
noNasalVowels _ o = length [ 1 | (P g a p m n,xps) <- o, isVowel m && n == Nasal]

-- no voiced stops
noVoicedStops :: Constraint
noVoicedStops _ o = length [ backness phone | (phone@(P g a p m n),xps) <- o, m == Stop Tenuis && g == Voiced]

-- no voicless resonants
noVoicelessResonants :: Constraint
noVoicelessResonants _ o = length [ 1 | (P g a p m n,_) <- o, not (isObstruent m) && g == Voiceless]

-- no voiced obstruents
noVoicedObstruents :: Constraint
noVoicedObstruents _ o = length [ 1 | (P g a p m n,_) <- o, isObstruent m && g == Voiced]

-- adjacent elements must agree in some feature f, takes {place, obsVoice, nasalObs, isṼN, nasal} as argument
agree :: Comp -> Constraint
agree f _ o = length [ 1 | [a,b] <- groups 2 (map fst o), not (f a b)]

-- linear order preservation/no metathesis (usually classed as a faithfulness constraint)
-- fix: [('b',[1]),('c',[2]),('a',[0])] gives 1 violation but should give 2
linearity :: Constraint
linearity _ o = length [ 1 | [as,bs] <- groups 2 (map snd o), or [ any (< a) bs | a <- as]]

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

-- no empty onset - fix: "sto" gives violation
onset :: Constraint
onset _ o = sum (map (f . take 2 . map sonorityOf) (syllables (map fst o)))
    where
        f :: [Int] -> Int
        f [x] = 1
        f [a,b] | a > b = 1 | otherwise = 0

-- Main

optimal :: Grammar -> String -> [Lexeme] -> [String]
optimal g i os = map toString (mask (optimalForms (map (eval g (toLexeme i)) os)) os)

prop_optimal :: Grammar -> String -> Harmony -> Lexeme -> Bool
prop_optimal g i n o = n <= eval g (toLexeme i) (head (mask (optimalForms [eval g (toLexeme i) o]) [o]))

-- quickCheck (prop_optimal *grammar* *input form* *harmony*)
-- theoretically should return a form of harmony greater than the inputed harmony if one exists

-- optimal *grammar* *input form* (gen *input form*)
-- theoretically should return the most harmonious output form(s) for the given grammar and input form

-- Examples

{-
optimal is the only function that needs to be called by the user
it takes a grammar, an input form (which is automatically indexed), and a list of indexed output forms
it returns a list of the most harmonious output forms (which are automatically unindexed)

examples:

example a)
> optimal [nasAgr, ident obsVoice] "amda" (map toLexeme ["ampa", "amda"])
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
> optimal [ident obsVoice, nasAgr] "amda" (map toLexeme ["ampa", "amda"])
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