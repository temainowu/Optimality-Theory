import Test.QuickCheck

type Phone = Char
-- should Phone be of type Char?
-- should it be String to account for phones that are more than one character long?
-- should it be [Place, Manner, ...] to represent phones more abstractly?
-- or not a list but a more abstract data type?

type FindFeature f = Phone -> f

type Comp = Phone -> Phone -> Bool

type Lexeme = [(Phone, [Int])]

type Constraint = Lexeme -> Lexeme -> Int 

type Fluxion = ([Int], Int)
-- Fluxions are used to represent the harmony of a form
-- the smaller the fluxion, the more harmonic the form
-- ([a₀,a₁,...,aₖ],n) represents the fluxion (a₀ε⁰+a₁ε¹+...+aₖεᵏ)ωⁿ
-- this is able to represent all possible finitely long integer fluxions
-- see fluxions.txt for more information on fluxions

type Grammar = [Constraint]

type PhoneClass = [Phone]
-- PhoneClass (= [Phone]) is used when refering to classes of phones
-- String (= [Phone]) is used when refering to a sequence of phones

data Phone' = P GlottalState Active Passive Manner
    deriving (Eq, Show)

data Passive = Superiolabial | Dental | Alveolar | Postalveolar | Palatal | Velar | Uvular | Pharyngeal | Epiglottal
    deriving (Eq, Show)

data Active = Inferiolabial | Apical | Laminal | Dorsal | Glottal
    deriving (Eq, Show)

data Manner = Stop | Fricative | Nasal | Trill | Tap | Approximant | HighVowel | MidHighVowel | MidVowel | MidLowVowel | LowVowel
    deriving (Eq, Show)

data GlottalState = Voiced | Voiceless
    deriving (Eq, Show)

-- Classes of sounds

{- vowel chart:
i		y		ɯ		u
	ɪ		ɵ		ʊ	
e		ø		ɤ		o
ɛ		œ	ə	ʌ		ɔ
æ		ɶ	ɐ	ɑ		ɒ
			a			

this is based on dr geoff lindey 's vowel chart but with the addition of ɶ and ɒ and considering æ to be a cardinal vowel.
-}

boundary = ".+"
universe = "pbmʙɸβwɱⱱfvʋtdnrɾszɬɮɹlʃʒɕʑʈɖɳɽʂʐɻɭcɟɲçʝjʎkgŋxɣɰʟieɛæɶɪyøœɵəɐaɯɤʌɑʊuoɔɒ"
obs = stop ++ fric
res = complement obs
vowel = "ieɛæɪyøœɶɵəɐaɯɤʌɑʊuoɔɒ"
consonant = complement vowel
unvoiced = "pɸftsɬʃʈʂcçkx"
voiced = complement unvoiced
rounded = "wyøœɵəɶɐaʊuoɔɒ"
unrounded = complement rounded
lax = "ɪɵʊəɐ"
tense = complement lax
lat = "ɬɮlɭʎʟ"
sib = "sz"

-- tongue places

api = "tdnszɹ" ++ "rɾɬɮlʃʒʈɖɳɽʂʐɻɭ" -- phones shouldn't be in multiple mutually exclusive classes
lam = "tdnszɹ" ++ "ɕʑ" -- most alveolar phones are underspecified for laminal/apical/dental in the IPA
dors = "cɟɲçʝjʎkgŋxɣɰʟ"

-- manners
stop = "pbtdʈɖcɟkg"
fric = "ɸβfvszɬɮʃʒɕʑʂʐçʝxɣ"
nas = "mɱnɳɲŋ"
tap = "ⱱɾɽ"
trill = "ʙr"
appr = "wʋɹlɻɭjɰʎʟ"
hi = "iɪyɵɯʊu"
mhi = "eøɤo"
mid = "ɛœəʌɔ"
mlo = "æɶɐɑɒ"
lo = "a"

-- places
lab = "pbmʙɸβ"
labdent = "ɱⱱfvʋ"
alv = "tdnrɾszɬɮɹl"
postalv = "ʃʒɕʑ"
pal = "ʈɖɳɽʂʐɻɭcɟɲçʝjʎieɛæɶɪyøœ"
vel = "kgŋxɣɰʟɯwɤʌɑʊuoɔɒ"

-- ɵəɐa are not given places because they are central

-- Auxiliary Functions

placeOf :: FindFeature Passive
placeOf x
    | x `elem` lab = Superiolabial
    | x `elem` labdent = Dental
    | x `elem` alv = Alveolar
    | x `elem` postalv = Postalveolar
    | x `elem` pal = Palatal
    | x `elem` vel = Velar

mannerOf :: FindFeature Manner
mannerOf x
    | x `elem` stop = Stop
    | x `elem` fric = Fricative
    | x `elem` nas = Nasal
    | x `elem` trill = Trill
    | x `elem` tap = Tap
    | x `elem` appr = Approximant
    | x `elem` hi = HighVowel
    | x `elem` mhi = MidHighVowel
    | x `elem` mid = MidVowel
    | x `elem` mlo = MidLowVowel
    | x `elem` lo = LowVowel

sonorityOf :: FindFeature Int
sonorityOf x
    | m == Stop = 0
    | m == Fricative = 1
    | m == Nasal = 2
    | m == Trill = 3
    | m == Tap = 4
    | m == Approximant = 5
    | m == HighVowel = 6
    | m == MidHighVowel = 7
    | m == MidVowel = 8
    | m == MidLowVowel = 9
    | m == LowVowel = 10
    where m = mannerOf x

-- % is the set difference operator
(%) :: PhoneClass -> PhoneClass -> PhoneClass
xs % ys = [x | x <- xs, x `notElem` ys]

-- complement of set
complement :: PhoneClass -> PhoneClass
complement xs = universe % xs

-- index "abc" = [('a',[0]),('b',[1]),('c',[2])] 
index :: String -> Lexeme
index xs = zip xs (map (: []) [0..])

-- unIndex [('a',[0, 1]),('b',[]),('c',[2, 3, 4])] = "abc"
unIndex :: Lexeme -> String
unIndex = filter (/= '+') . map fst

affix :: String -> String -> String
affix xs ys = xs ++ '+' : ys

groups :: Int -> [a] -> [[a]]
groups n xs = [take n (drop i xs) | i <- [0..length xs - n]]

syllables :: String -> [String]
syllables [] = []
syllables ('.':xs) = syllables xs
syllables xs = (\ f (a,as) -> a : f as) syllables (break (== '.') xs)

sizeOfCoda :: String -> Int
sizeOfCoda [] = 0
sizeOfCoda [x] = 0
sizeOfCoda (x:y:xs)
    | sonorityOf x > sonorityOf y = 1 + sizeOfCoda (y:xs)
    | otherwise = 0 + sizeOfCoda (y:xs)

(≻) :: Lexeme -> Lexeme -> Grammar -> String -> Bool
x ≻ y = \g i -> fluxionLEq (eval g (index i) x) (eval g (index i) y)

{- not possible because Constraint can't be an instance of Eq
(⪢) :: Constraint -> Constraint -> Grammar -> Bool
x ⪢ y = \g -> find g x < find g y

find :: Eq a => [a] -> a -> Int
find xs y = head [i | (i,x) <- zip [0..] xs, y == x]
-}

-- Fluxions

fluxionLEq :: Fluxion -> Fluxion -> Bool
fluxionLEq ([],n) ([],m) = True
fluxionLEq ([],n) (y:ys,m)
    | y /= 0 = y > 0
    | otherwise = fluxionLEq ([],0) (ys,m-1)
fluxionLEq (x:xs,n) ([],m)
    | x /= 0 = x < 0
    | otherwise = fluxionLEq (xs,n-1) ([],0)
fluxionLEq (x:xs,n) (y:ys,m)
    | x == 0 = fluxionLEq (xs,n-1) (y:ys,m)
    | y == 0 = fluxionLEq (x:xs,n) (ys,m-1)
    | n < m = y > 0
    | n > m = x < 0
    | n == m = case () of
       () | x < y -> True
          | x > y -> False
          | otherwise -> fluxionLEq (xs,n-1) (ys,m-1)

-- equivalent to fluxionLEq written by cosmo bobak (https://github.com/cosmobobak):

-- takes two fluxions to representations with the same "n" and equal-length lists
doubleNormalise :: ([Int], Int) -> ([Int], Int) -> (([Int], Int), ([Int], Int))
doubleNormalise ([], n) ([], m) = doubleNormalise ([0], n) ([0], m)
doubleNormalise r@(xs, n) l@(ys, m)
  -- if the offsets are the same, we just need to pad the shorter list with zeros
  | n == m = ((xs ++ replicate xsMakeUp 0, n), (ys ++ replicate ysMakeUp 0, m))
  -- if the left fluxion has a smaller offset, we need to left-pad it with zeros
  | n < m = doubleNormalise (replicate (m - n) 0 ++ xs, m) l
  -- if the right fluxion has a smaller offset, we need to left-pad it with zeros
  | otherwise = doubleNormalise r (replicate (n - m) 0 ++ ys, n)
  where
    xsMakeUp = max (length ys - length xs) 0
    ysMakeUp = max (length xs - length ys) 0

leq :: Fluxion -> Fluxion -> Bool
leq x y = uncurry (\(xs, _) (ys, _) -> xs <= ys) (doubleNormalise x y)

prop_leq :: Fluxion -> Fluxion -> Bool
prop_leq x y = leq x y == fluxionLEq x y

-- in this program all fluxions are of the same length, do not contain negative values, and the right of the pair is always 0
-- so some of the above cases are redundant

smallestFluxion :: [Fluxion] -> Fluxion
smallestFluxion [] = error "no fluxions"
smallestFluxion [x] = x
smallestFluxion (x:xs)
    | fluxionLEq x (smallestFluxion xs) = x
    | otherwise = smallestFluxion xs

-- fluxionLessThanOrEqual x (y:ys) = x

smallestFluxions :: [Fluxion] -> [Bool]
smallestFluxions xs = map (== smallestFluxion xs) xs

mask :: [Bool] -> [a] -> [a]
mask [] _ = []
mask _ [] = []
mask (True:bs) (x:xs) = x : mask bs xs
mask (False:bs) (x:xs) = mask bs xs

eval :: Grammar -> Lexeme -> Lexeme -> Fluxion
eval g i o = (map (\ f -> f i o) g,0)

-- fix: does not generate all possible forms
gen :: String -> [String]
gen [] = []
gen (x:xs) = map (: xs) (complement [x]) ++ map (x :) (gen xs)

-- IO comparisons:

-- same place
place :: Comp
place a b
    | placeOf a == placeOf b = True
    | otherwise = False

-- same voicing of obstuents
obsVoice :: Comp
obsVoice a b
    | a `elem` voiced % res && b `elem` voiced % res = True
    | a `elem` universe % voiced && b `elem` universe % voiced = True
    | otherwise = False

-- I->O comparisons:

-- nasality preservation
nasal :: Comp
nasal a b
    | a `notElem` nas = True
    | a `elem` nas && b `elem` nas = True
    | otherwise = False

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

-- nasal agrees with place of following obstruent
nasAgr :: Constraint
nasAgr _ o = sum [auxNasAgr a b | [a,b] <- groups 2 (unIndex o)]
    where
        auxNasAgr a b
            | a `elem` nas && b `elem` obs && place a b = 0
            | a `notElem` nas || b `notElem` obs = 0
            | otherwise = 1

-- linear order preservation/no metathesis (usually classed as a faithfulness constraint)
-- fix: [('b',[1]),('c',[2]),('a',[0])] gives 1 violation but should give 2
linearity :: Constraint
linearity _ o = length [ 1 | [as,bs] <- groups 2 (map snd o), or [ any (< a) bs | a <- as]]

-- no non-back rounded vowels
noFrontRound :: Constraint
noFrontRound _ o = length [ 1 | (x,xps) <- o, x `notElem` vel && x `elem` vowel && x `elem` rounded]

-- syllable constraints:
-- a syllable is too hard to define so a lot of the following constraints are just approximations that will likely never be perfect

-- no complex syllables
noComplex :: Constraint
noComplex _ o = length [ x | x <- groups 3 (unIndex o), '.' `notElem` x]

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

mostOptimal :: Grammar -> String -> [Lexeme] -> [String]
mostOptimal g i os = map unIndex (mask (smallestFluxions (map (eval g (index i)) os)) os)

prop_mostOptimal :: Grammar -> String -> Fluxion -> Lexeme -> Bool
prop_mostOptimal g i n o = fluxionLEq n (eval g (index i) (head (mask (smallestFluxions [eval g (index i) o]) [o])))

-- quickCheck (prop_mostOptimal *grammar* *input form* *harmony*)
-- theoretically should return a form of harmony greater than the inputed harmony if one exists

-- Examples

{-
mostOptimal is the only function that needs to be called by the user
it takes a grammar, an input form (which is automatically indexed), and a list of indexed output forms
it returns a list of the most harmonious output forms (which are automatically unindexed)

examples:

example a)
> mostOptimal [nasAgr, ident obsVoice] "amda" [index "ampa", index "amda"]
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
> mostOptimal [ident obsVoice, nasAgr] "amda" [index "ampa", index "amda"]
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