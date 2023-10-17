import Test.QuickCheck

type Phone = Char
-- should Phone be of type Char?
-- should it be String to account for phones that are more than one character long?
-- should it be [Place, Manner, ...] to represent phones more abstractly?

type FindFeature f = Phone -> f

type Comp = Phone -> Phone -> Bool

type Lexeme = [(Phone, [Int])]

type Constraint = Lexeme -> Lexeme -> Int

type Fluxion = ([Int], Int)
-- Fluxions are used to represent the harmony of a form
-- the smaller the fluxion, the more harmonic the form
-- see fluxions.txt for more information on fluxions

type Grammar = [Constraint]

type PhoneClass = [Phone]
-- PhoneClass (= [Phone]) is used when refering to classes of phones
-- String (= [Phone]) is used when refering to a sequence of phones

data Place = Labial | LabioDental | Alveolar | Palatal | Velar | LabioVelar deriving (Eq, Show)

data Manner = Stop | Fricative | Nasal | Trill | Tap | Approximant | Vowel deriving (Eq, Show)

-- Classes of sounds

boundaries = ".+"
universe = "pbmʙɸβwɱⱱfvʋtdnrɾszɬɮɹlcɟɲçʝjʎkgŋxɣɰʟieɛæɪyøœɵəɐaɯɤʌɑʊuoɔɒ"
obs = stop ++ fric
res = complement obs
vowel = "ieɛæɪyøœɵəɐaɯɤʌɑʊuoɔɒ"
consonant = complement vowel
voiced = "bβvdzɮɟʝgɣ" ++ res
unvoiced = complement voiced
rounded = "wyøœɵəɐaʊuoɔɒ"
unrounded = complement rounded
lat = "ɬɮlʎʟ"
lax = "ɪʊəɐ"
tense = complement lax

-- manners
stop = "pbtdcɟkg"
fric = "ɸβfvszɬɮçʝxɣ"
nas = "mɱnɲŋ"
tap = "ⱱɾ"
trill = "ʙr"
appr = "wʋɹljɰʎʟ"

hi = "iɪyɵɯʊu"
mhi = "eøɤo"
mlo = "ɛœʌɔ"
lo = "æɐaɑɒ"

-- ə is not given a manner because it is true mid

-- places
lab = "pbmʙɸβ"
labdent = "ɱⱱfvʋ"
alv = "tdnrɾszɬɮɹl"
pal = "cɟɲçʝjʎieɛæɪyøœ"
vel = "kgŋxɣɰʟɯwɤʌɑʊuoɔɒ"

-- ɵəɐa are not given places because they are central

-- Auxiliary Functions

placeOf :: FindFeature Place
placeOf x
    | x `elem` lab = Labial
    | x `elem` labdent = LabioDental
    | x `elem` alv = Alveolar
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
    | x `elem` vowel = Vowel

{-
sonoranceOf :: FindFeature Int
sonoranceOf x 
    | m == Stop = 0
    | m == Fricative = 1
    | m == Nasal = 2
    | m == Trill = 3
    | m == Tap = 4 
    | m == Approximant = 5
    | m == Vowel = 6
    where m = mannerOf x
-}

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

-- Fluxions

fluxionLessThanOrEqual :: Fluxion -> Fluxion -> Bool
fluxionLessThanOrEqual ([],0) ([],0) = True
fluxionLessThanOrEqual ([],n) (y:ys,m)
    | y > 0 = True
    | y < 0 = False
    | otherwise = fluxionLessThanOrEqual ([],0) (ys,m-1)
fluxionLessThanOrEqual (x:xs,n) ([],m)
    | x > 0 = False
    | x < 0 = True
    | otherwise = fluxionLessThanOrEqual (xs,n-1) ([],0)
fluxionLessThanOrEqual (x:xs,n) (y:ys,m)
    | n == m = case () of
       () | x < y -> True
          | x > y -> False
          | otherwise -> fluxionLessThanOrEqual (xs,n-1) (ys,m-1)
    | otherwise = fluxionLessThanOrEqual (x-1:xs,n) (y-1:ys,m)

-- in this program all fluxions are of the same length, do not contain negative values, and the right of the pair is always 0
-- so some of the above cases are redundant

smallestFluxion :: [Fluxion] -> Fluxion
smallestFluxion [] = ([1], 1)
smallestFluxion [x] = x
smallestFluxion (x:xs)
    | fluxionLessThanOrEqual x (smallestFluxion xs) = x
    | otherwise = smallestFluxion xs

-- fluxionLessThanOrEqual x (y:ys) = x

smallestFluxions :: [Fluxion] -> [Bool]
smallestFluxions xs = map (== smallestFluxion xs) xs

mask :: [Bool] -> [a] -> [a]
mask [] _ = []
mask _ [] = []
mask (True:bs) (x:xs) = x : mask bs xs
mask (False:bs) (x:xs) = mask bs xs

harmony :: Grammar -> Lexeme -> Lexeme -> Fluxion
harmony g i o = (map (\ f -> f i o) g,0)

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
maxi i o = length [y | (y,[yp]) <- i, or [yp `elem` xps | (x,xps) <- o]]

-- no epenthesis
dep :: Constraint
dep i o = length [x | (x,xps) <- o, null xps]

-- feature preservation
ident :: Comp -> Constraint
ident f i o = length [x | (x,xps) <- o, (y,[yp]) <- i, not (f x y), yp `elem` xps]

-- no coalescence
uniformity :: Constraint
uniformity i o = length [x | (x,xps) <- o, length xps > 1]

-- markedness constraints (ignore first argument)

-- nasal agrees with place of following obstruent
nasAgr :: Constraint
nasAgr _ o = sum [auxNasAgr a b | [a,b] <- groups 2 (unIndex o)]
    where
        auxNasAgr a b
            | a `elem` nas && b `elem` obs && place a b = 0
            | a `notElem` nas || b `notElem` obs = 0
            | otherwise = 1

-- syllable constraints:

-- no complex syllables
noComplex :: Constraint
noComplex _ o = length [ x | x <- groups 3 (unIndex o), '.' `notElem` x]

-- Main

mostHarmonious :: Grammar -> String -> [Lexeme] -> [String]
mostHarmonious g i os = map unIndex (mask (smallestFluxions (map (harmony g (index i)) os)) os)

prop_mostHarmonious :: Grammar -> String -> Fluxion -> Lexeme -> Bool
prop_mostHarmonious g i n o = fluxionLessThanOrEqual n (harmony g (index i) (head (mask (smallestFluxions [harmony g (index i) o]) [o])))

-- quickCheck (prop_mostHarmonious *grammar* *input form* *harmony*)
-- theorhetically should return a form of harmony greater than the inputed harmony if one exists

-- Examples

{-
mostHarmonious is the only function that needs to be called by the user
it takes a grammar, an input form (which is automatically indexed), and a list of indexed output forms
it returns a list of the most harmonious output forms (which are automatically unindexed)

examples:

example a)
> mostHarmonious [nasAgr, ident obsVoice] "amda" [index "ampa", index "amda"]
["ampa"]

nasAgr dominates ident obsVoice, 
so the output form is "ampa" because 'm' agrees with 'p' in place.

tableu:

  amda | nasAgr | ident obsVoice |
_______|________|________________|
  amda |   *!   |                |
_______|________|________________|
> ampa |        |       *        |
_______|________|________________|

example b)
> mostHarmonious [ident obsVoice, nasAgr] "amda" [index "ampa", index "amda"]
["amda"]

identIO obsVoice dominates nasAgr,
so the output form is "amda" because 'd' is voiced,
 which matches the voicing of the corresponding input segment 'd'.

tableu:

  amda | ident obsVoice | nasAgr |
_______|________________|________|
> amda |                |   *    |
_______|________________|________|
  ampa |       *!       |        |
_______|________________|________|

the forms; index "amba", index "anda", index "embi", [('n',1),('d',2)], and [] 
are all more harmonious forms in both examples,
but they were not chosen because they were not in the input list.

-}