import Test.QuickCheck
import GHC.Data.FastString.Env (FastStringEnv)

type FindFeature f = Char -> f

type Comp = (Char,Char) -> Bool

type Lexeme = [(Char, [Int])]
-- should the first element of the tuples be of type Char?
-- should it be String to account for phones that are more than one character long?
-- should it be [Place, Manner, ...] to represent phones more abstractly?

type Constraint = Lexeme -> Lexeme -> Int

type Fluxion = [Int]

type Grammar = [Constraint]

type PhoneClass = [Char]
-- PhoneClass (= [Char]) is used when refering to classes of phones
-- String (= [Char]) is used when refering to a sequence of phones

data Place = Labial | LabioDental | Alveolar | Palatal | Velar | LabioVelar deriving (Eq, Show)

data Manner = Stop | Fricative | Nasal | Trill | Tap | Approximant | Vowel deriving (Eq, Show)

-- Classes of sounds

universe = "pbmʙɸβwɱⱱfvʋtdnrɾszɬɮɹlcɟɲçʝjʎkgŋxɣɰʟieɛæɪyøœɵəɐaɯɤʌɑʊuoɔɒ"
obs = stop ++ fric
res = universe % obs
vowel = "ieɛæɪyøœɵəɐaɯɤʌɑʊuoɔɒ"
consonant = universe % vowel
voiced = "bβvdzɮɟʝgɣ" ++ res
unvoiced = universe % voiced
rounded = "wyøœɵəɐaʊuoɔɒ"
unrounded = universe % rounded
lat = "ɬɮlʎʟ"
lax = "ɪʊəɐ"

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

-- index "abc" = [('a',[0]),('b',[1]),('c',[2])] 
index :: String -> Lexeme
index xs = zip xs (map (: []) [0..])

unIndex :: Lexeme -> String
unIndex = map fst

-- Fluxions

fluxionLessThanOrEqual :: Fluxion -> Fluxion -> Bool
fluxionLessThanOrEqual [] [] = True
fluxionLessThanOrEqual (x:xs) (y:ys)
    | x == y = fluxionLessThanOrEqual xs ys
    | x < y = True
    | x > y = False

smallestFluxion :: [Fluxion] -> Fluxion
smallestFluxion [] = [] !! 1
smallestFluxion [x] = x
smallestFluxion (x:xs)
    | fluxionLessThanOrEqual x (smallestFluxion xs) = x
    | otherwise = smallestFluxion xs

smallestFluxions :: [Fluxion] -> [Bool]
smallestFluxions xs = map (\ x -> x == smallestFluxion xs) xs

mask :: [Bool] -> [a] -> [a]
mask [] _ = []
mask _ [] = []
mask (True:bs) (x:xs) = x : mask bs xs
mask (False:bs) (x:xs) = mask bs xs

measure :: Grammar -> Lexeme -> Lexeme -> Fluxion
measure fs i o = map (\ f -> f i o) fs

-- Comparisons

place :: Comp
place (a,b)
    | placeOf a == placeOf b = True
    | otherwise = False

obsVoice :: Comp
obsVoice (a,b)
    | a `elem` voiced % res && b `elem` voiced % res = True
    | a `elem` universe % voiced && b `elem` universe % voiced = True
    | otherwise = False

-- Constraints

-- faithfulness constraints (care about first argument)

-- no deletion
maxi :: Constraint
maxi i o = length [y | (y,[yp]) <- i, or [yp `elem` xps | (x,xps) <- o]]

-- no epenthesis
dep :: Constraint
dep i o = length [x | (x,xps) <- o, null xps]

-- feature preservation
ident :: Comp -> Constraint
ident f i o = length [x | (x,xps) <- o, (y,[yp]) <- i, not (f (x,y)), yp `elem` xps]

-- no coalescence
uniformity :: Constraint
uniformity i o = length [x | (x,xps) <- o, length xps > 1]

-- markedness constraints (ignore first argument)

-- nasal agrees with place of following obstruent
nasAgr :: Constraint
nasAgr _ o = sum [auxNasAgr (a,b) | ((a,_),(b,_)) <- zip o (drop 1 o)]
    where
        auxNasAgr (a, b)
            | a `elem` nas && b `elem` obs && place (a,b) = 0
            | a `notElem` nas || b `notElem` obs = 0
            | otherwise = 1

-- Main

mostHarmonious :: Grammar -> String -> [Lexeme] -> [String]
mostHarmonious g i os = map unIndex (mask (smallestFluxions (map (measure g (index i)) os)) os)

prop_mostHarmonious :: Grammar -> String -> Fluxion -> Lexeme -> Bool
prop_mostHarmonious g i n o = fluxionLessThanOrEqual (measure g (index i) (head (mask (smallestFluxions [measure g (index i) o]) [o]))) n

-- quickCheck (prop_mostHarmonios *grammar* *input form* *harmony*)
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