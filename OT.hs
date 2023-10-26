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

this is based on dr geoff lindey 's vowel chart 
but with the addition of ɶ, ɨ, and ɒ 
and considering æ to be a cardinal vowel.

vowel = "ieɛæɪyøœɶɵəɐaɯɤʌɑʊuoɔɒ"
lax = "ɪɵʊəɐ"
-}


-- Auxiliary Functions

isVowel :: Manner -> Bool
isVowel m = m `elem` [Vowel High Nasal, Vowel MidHigh Nasal, Vowel Mid Nasal, Vowel MidLow Nasal, Vowel Low Nasal, Vowel High Oral, Vowel MidHigh Oral, Vowel Mid Oral, Vowel MidLow Oral, Vowel Low Oral]

isNasal :: Manner -> Bool
isNasal m = m `elem` [Vowel High Nasal, Vowel MidHigh Nasal, Vowel Mid Nasal, Vowel MidLow Nasal, Vowel Low Nasal, Stop Nasal]

isRounded :: Active -> Bool
isRounded (Tongue (Dorsal Rounded) _) = True
isRounded _ = False

sonorityOf :: Phone -> Int
sonorityOf (P _ _ _ m)
    | m == Click = 0
    | m == Stop Oral = 1
    | m == Affricate Sibilant = 2
    | m == Affricate NonSibilant = 2
    | m == Fricative Sibilant = 3
    | m == Fricative NonSibilant = 3
    | m == Tap = 4
    | m == Trill = 5
    | m == Stop Nasal = 6
    | m == Approximant = 7
    | m == Vowel High Nasal || m == Vowel High Oral = 8
    | m == Vowel MidHigh Nasal || m == Vowel MidHigh Oral = 9
    | m == Vowel Mid Nasal || m == Vowel Mid Oral = 10
    | m == Vowel MidLow Nasal || m == Vowel MidLow Oral = 11
    | m == Vowel Low Nasal || m == Vowel Low Oral = 12

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
toString xs = filter (/= '+') (concatMap (phoneToString . fst) xs)

unIndex :: Lexeme -> [Phone]
unIndex = filter (/= MorphemeBoundary) . map fst

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
place (P g0 a0 p0 m0) (P g1 a1 p1 m1)
    | (p0 == p1) && (a0 == a1) = True
    | otherwise = False

-- same voicing of obstuents
obsVoice :: Comp
obsVoice (P g0 a0 p0 m0) (P g1 a1 p1 m1)
    | g0 /= g1 && isObstruent m0 && isObstruent m1 = False
    | otherwise = True

-- nasal agrees in place with following obstruent
nasalObs :: Comp
nasalObs a@(P g0 a0 p0 m0) b@(P g1 a1 p1 m1)
            | m0 == Stop Nasal && isObstruent m1 && place a b = True
            | not (m0 == Stop Nasal && isObstruent m1) = True
            | otherwise = False

-- I->O comparisons:

-- nasality preservation
nasal :: Comp
nasal (P g0 a0 p0 m0) (P g1 a1 p1 m1)
    | isNasal m0 && not (isNasal m1) = False
    | otherwise = True

-- Constraints

-- faithfulness constraints (care about both arguments)

-- no deletion
maxi :: Constraint
maxi i o = length [ 1 | (y,[yp]) <- i, or [yp `elem` xps | (x,xps) <- o]]

-- no epenthesis
dep :: Constraint
dep i o = length [ 1 | (x,xps) <- o, null xps]

-- feature similarity/preservation
ident :: Comp -> Constraint
ident f i o = length [ 1 | (x,xps) <- o, (y,[yp]) <- i, not (f x y), yp `elem` xps]
-- "ident nasal" = "IdentI->O(nasal)" in OT
-- "ident place" = "IdentIO(place)" in OT

-- no coalescence
uniformity :: Constraint
uniformity i o = length [ 1 | (x,xps) <- o, length xps > 1]

-- markedness constraints (ignore first argument)

-- adjacent elements must agree in some feature f
agree :: Comp -> Constraint
agree f _ o = length [ 1 | [a,b] <- groups 2 (unIndex o), not (f a b)]

-- linear order preservation/no metathesis (usually classed as a faithfulness constraint)
-- fix: [('b',[1]),('c',[2]),('a',[0])] gives 1 violation but should give 2
linearity :: Constraint
linearity _ o = length [ 1 | [as,bs] <- groups 2 (map snd o), or [ any (< a) bs | a <- as]]

-- no non-back rounded vowels
noFrontRound :: Constraint
noFrontRound _ o = length [ 1 | (P g a p m,xps) <- o, p /= Velar && isVowel m && isRounded a]

-- syllable constraints:
-- a syllable is too hard to define so a lot of the following constraints are just approximations that will likely never be perfect

-- no complex syllables
noComplex :: Constraint
noComplex _ o = length [ x | x <- groups 3 (unIndex o), SyllableBoundary `notElem` x]

-- no coda
noCoda :: Constraint
noCoda _ o = sum (map sizeOfCoda (syllables (unIndex o)))

-- no empty onset - fix: "sto" gives violation
onset :: Constraint
onset _ o = sum (map (f . take 2 . map sonorityOf) (syllables (unIndex o)))
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

-- Examples

{-
optimal is the only function that needs to be called by the user
it takes a grammar, an input form (which is automatically indexed), and a list of indexed output forms
it returns a list of the most harmonious output forms (which are automatically unindexed)

examples:

example a)
> optimal [nasAgr, ident obsVoice] "amda" [index "ampa", index "amda"]
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
> optimal [ident obsVoice, nasAgr] "amda" [index "ampa", index "amda"]
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

the forms; index "amba", index "anda", index "embi", [('n',[1]),('d',[2])], and [] 
are all more harmonious forms in both examples,
but they were not chosen because they were not in the list of possible outputs.

-}