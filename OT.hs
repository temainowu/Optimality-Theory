import GHC.CmmToAsm.AArch64.Instr (x0)


-- Classes of sounds
type FindFeature f = Char -> f

universe = "pbmʙɸβwɱⱱfvʋtdnrɾszɹeuioasghjklzx"
obs = stop ++ fric
res = universe % obs
vowel = "aeiou"
consonant = universe % vowel
voiced = "bβvdz" ++ res
unvoiced = universe % voiced

-- manners
stop = "pbtdkg"
fric = "ɸβfvszhx"
nas = "mɱn"
tap = "ⱱɾ"
trill = "ʙr"
appr = "wʋɹ"

-- places
lab = "pbmʙɸβ"
labdent = "ɱⱱfvʋ"
alv = "tdnrɾszɹ"
vel = "kgx"
labvel = "wuo"

-- Auxiliary Functions

data Place = Labial | LabioDental | Alveolar | Palatal | Velar | LabioVelar deriving (Eq, Show)
data Manner = Stop | Fricative | Nasal | Trill | Tap | Approximant | Vowel deriving (Eq, Show)

placeOf :: FindFeature Place
placeOf x 
    | x `elem` lab = Labial
    | x `elem` labdent = LabioDental
    | x `elem` alv = Alveolar
    | x `elem` vel = Velar
    | x `elem` labvel = LabioVelar

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

(%) :: [Char] -> [Char] -> [Char]
xs % ys = [x | x <- xs, x `notElem` ys]

--

rect :: Int -> Int
rect x
    | x < 0 = 0
    | otherwise = x

pos :: [[Int]] -> [Int] -> [Int]
pos xs a = [p | p <- [0..length xs - 1], xs !! p == a]

fluxionLessThan :: [Int] -> [Int] -> Bool
fluxionLessThan [] [] = True
fluxionLessThan (x:xs) (y:ys)
    | x == y = fluxionLessThan xs ys
    | x < y = True
    | x > y = False

smallestFluxion :: [[Int]] -> [Int]
smallestFluxion [] = [] !! 1
smallestFluxion [x] = x
smallestFluxion (x:xs)
    | fluxionLessThan x (smallestFluxion xs) = x
    | otherwise = smallestFluxion xs

measure :: [Constraint] -> [Char] -> [Char] -> [Int]
measure fs i o = map (\ f -> f i o) fs

-- Comparisons

type Comp = (Char,Char) -> Bool

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

type Constraint = [Char] -> [Char] -> Int

-- faithfulness constraints (care about first argument)

maxIO :: Constraint
maxIO i o = rect (length i - length o)

depIO :: Constraint
depIO i o = rect (length o - length i)

identIO :: Comp -> Constraint
identIO f i o = length [x | x <- zip i o, not (f x)]

-- markedness constraints (ignore first argument)

nasAgr :: Constraint
nasAgr _ o = sum [auxNasAgr x | x <- zip o (drop 1 o)]
    where auxNasAgr (a, b) | a `elem` nas && b `elem` obs && place (a,b) = 0 | a `notElem` nas || b `notElem` obs = 0 | otherwise = 1

-- Main

mostHarmonious :: [Constraint] -> [Char] -> [[Char]] -> [[Char]]
mostHarmonious g i o = [o !! p | p <- pos measuredOutput (smallestFluxion measuredOutput)]
    where measuredOutput = map (measure g i) o