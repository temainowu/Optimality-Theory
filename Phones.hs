module OptimalityTheory.Phones where

data Phone = P GlottalState Active Passive Manner Nasality | SyllableBoundary | MorphemeBoundary | WordBoundary
    deriving (Eq)
    
instance Show Phone where
    show = phoneToString

data Passive = Superiolabial | Dental | Alveolar | Postalveolar | Palatal | PalatoVelar | Velar | Uvular | Pharyngeal | NoPassive
    deriving (Eq, Show)

instance Enum Passive where
    fromEnum Superiolabial = 0
    fromEnum Dental = 1
    fromEnum Alveolar = 2
    fromEnum Postalveolar = 3
    fromEnum Palatal = 4
    fromEnum PalatoVelar = 5
    fromEnum Velar = 6
    fromEnum Uvular = 7
    fromEnum Pharyngeal = 8
    fromEnum NoPassive = 9
    toEnum _ = NoPassive

data Active = Inferiolabial | Tongue Rounding TonguePlace Laterality | Epiglottal | NoActive
    deriving (Eq, Show)

instance Enum Active where
    fromEnum Inferiolabial = 0
    fromEnum (Tongue _ Apical _) = 1
    fromEnum (Tongue _ Laminal _) = 2
    fromEnum (Tongue _ Dorsal _) = 3
    fromEnum Epiglottal = 4
    fromEnum NoActive = 5
    toEnum _ = NoActive


data TonguePlace = Apical | Laminal | Dorsal
    deriving (Eq, Show)

data Manner = Click | Stop Release | Fricative Sibilance | NasalStop | Trill | Tap | Approximant | Vowel Height
    deriving (Eq, Show)

data Height = High | MidHigh | Mid | MidLow | Low
    deriving (Eq, Show)

data Sibilance = Sibilant | NonSibilant
    deriving (Eq, Show)

data Laterality = Lateral | Central
    deriving (Eq, Show)

data GlottalState = Voiced | Voiceless | Creaky | Breathy | Closed | VoicedIngressive
    deriving (Eq, Show)

data Rounding = Rounded | Unrounded
    deriving (Eq, Show)

data Nasality = Nasal | Oral
    deriving (Eq, Show)

data Release = Aspirated | Fricated Sibilance | Tenuis | Unreleased
    deriving (Eq, Show)

universe :: String
universe = "pbmʙɸβɱⱱfvʋθðtdnrɾɺszɬɮɹlʃʒɕʑʈɖɳɽʂʐɻɭcɟɲçʝjɥʎkgŋxʍɣɰwʟqɢɴʀχʁħʕhɦʔʜʢʡʘǀǃ‼ǂǁɓɗʄɠʛieɛæɶɪyøœɵəɐaɯɤʌɑʊuoɔɒ"
lat = "ɺɬɮlɭʎʟǁ"
rounded = "ʍwɥyøœʏɵɞɶʉʊuoɔɒ"
sib = "szʃʒɕʑʂʐ"

-- glottal states
unvoiced = "pɸfθtsɬʃʈʂcçkxʍqχħhʜʢʡʘǀǃ‼ǂǁ"
voiced = "bmʙβɱⱱvʋðdnrɾɺzɮɹlʒɕʑɖɳɽʐɻɭɟɲʝjɥʎgŋɣɰwʟɢɴʀʁʕieɛæɪyøœɶʏɨɘəɐaʉɵɞɯɤʌɑʊuoɔɒ"
creaky = ""
breathy = "ɦ"
closed = "ʔ"
ingressive = "ɓɗʄɠʛ"
asp = ""

-- active articulators
inflab = "pbmʙɸβɱⱱfvʋʘɓ"
api = "θðtdnrɾɺszɬɮɹlʃʒʈɖɳɽʂʐɻɭǀǃ‼ǁɗ"
lam = "ɕʑǂ"
dors = "cɟɲçʝjɥʎkgŋxʍɣɰwʟqɢɴʀχʁħʕʄɠʛieɛæɪyøœɶʏɨɘəɐaʉɵɞɯɤʌɑʊuoɔɒ"
epi = "ʜʢʡ"

-- passive articulators
suplab = "pbmʙɸβʘɓ"
dent = "ɱⱱfvʋθðǀ"
alv = "tdnrɾɺszɬɮɹlǁɗ"
postalv = "ʃʒɕʑǃǂ"
pal = "ʈɖɳɽʂʐɻɭcɟɲçʝjɥʎ‼ʄieɛæɪyøœɶʏ"
cent = "ɨʉɘɵəɞɐa"
vel = "kgŋxʍɣɰwʟɠɯɤʌɑʊuoɔɒ"
uvul = "qɢɴʀχʁʛ"
phar = "ħʕʜʢʡ"

-- manners
click = "ʘǀǃ‼ǂǁ"
stop = "pbtdʈɖcɟkgqɢʔʡɓɗʄɠʛ"
fric = "ɸβʍfvθðszɬɮʃʒɕʑʂʐçʝxɣχʁħʕhɦʜʢ"
nas = "mɱnɳɲŋɴ"
tap = "ⱱɾɽɺ"
trill = "ʙrʀ"
appr = "ʋɹlɻɭjɥɰwʎʟ"
hi = "iɪʏyɨʉɯʊu"
mhi = "eøɘɵɤo"
mid = "ɛœəɞʌɔ"
mlo = "æɶɐɑɒ"
lo = "a"

-- \\ is the set difference operator
(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = [x | x <- xs, x `notElem` ys]

-- complement of set
complement :: [Char] -> [Char]
complement xs = universe \\ xs

isObstruent :: Manner -> Bool
isObstruent m = m `elem` [Click, Stop Tenuis, Stop Aspirated, Stop Unreleased, Fricative Sibilant, Fricative NonSibilant, Stop (Fricated Sibilant), Stop (Fricated NonSibilant)]

passiveOf :: Char -> Passive
passiveOf x
    | x `elem` suplab = Superiolabial
    | x `elem` dent = Dental
    | x `elem` alv = Alveolar
    | x `elem` postalv = Postalveolar
    | x `elem` pal = Palatal
    | x `elem` cent = PalatoVelar
    | x `elem` vel = Velar
    | x `elem` uvul = Uvular
    | x `elem` phar = Pharyngeal
    | otherwise = NoPassive

activeOf :: Char -> Active
activeOf x
    | x `elem` inflab = Inferiolabial
    | x `elem` lat = case () of
         () | x `elem` api && x `notElem` rounded -> Tongue Unrounded Apical Lateral
            | x `elem` api && x `elem` rounded -> Tongue Rounded Apical Lateral
            | x `elem` lam && x `notElem` rounded -> Tongue Unrounded Laminal Lateral
            | x `elem` lam && x `elem` rounded -> Tongue Rounded Laminal Lateral
            | x `elem` dors && x `notElem` rounded -> Tongue Unrounded Dorsal Lateral
            | x `elem` dors && x `elem` rounded -> Tongue Rounded Dorsal Lateral
    | x `elem` api && x `notElem` rounded = Tongue Unrounded Apical Central
    | x `elem` api && x `elem` rounded = Tongue Rounded Apical Central
    | x `elem` lam && x `notElem` rounded = Tongue Unrounded Laminal Central
    | x `elem` lam && x `elem` rounded = Tongue Rounded Laminal Central
    | x `elem` dors && x `notElem` rounded = Tongue Unrounded Dorsal Central
    | x `elem` dors && x `elem` rounded = Tongue Rounded Dorsal Central
    | x `elem` epi = Epiglottal
    | otherwise = NoActive

glottalStateOf :: Char -> GlottalState
glottalStateOf x
    | x `elem` voiced = Voiced
    | x `elem` unvoiced = Voiceless
    | x `elem` creaky = Creaky
    | x `elem` breathy = Breathy
    | x `elem` closed = Closed
    | x `elem` ingressive = VoicedIngressive

mannerOf :: Char -> Manner
mannerOf x
    | x `elem` click = Click
    | x `elem` stop && x `elem` asp = Stop Aspirated
    | x `elem` stop = Stop Tenuis
    | x `elem` fric && x `elem` sib = Fricative Sibilant
    | x `elem` fric && x `notElem` sib = Fricative NonSibilant
    | x `elem` nas = NasalStop
    | x `elem` trill = Trill
    | x `elem` tap = Tap
    | x `elem` appr = Approximant
    | x `elem` hi = Vowel High
    | x `elem` mhi = Vowel MidHigh
    | x `elem` mid = Vowel Mid
    | x `elem` mlo = Vowel MidLow
    | x `elem` lo = Vowel Low

nasalityOf :: Char -> Nasality
nasalityOf x
    | x `elem` nas = Nasal
    | otherwise = Oral

charToPhone :: Char -> Phone
charToPhone '.' = SyllableBoundary
charToPhone '+' = MorphemeBoundary
charToPhone '#' = WordBoundary
charToPhone x = P (glottalStateOf x) (activeOf x) (passiveOf x) (mannerOf x) (nasalityOf x)


phoneToString :: Phone -> String
phoneToString WordBoundary = "#"
phoneToString MorphemeBoundary = "+"
phoneToString SyllableBoundary = "."
-- Errors
phoneToString (P _                  _                         _             NasalStop                  Oral ) = error "No oral nasal stops"
phoneToString (P _                  (Tongue _ Dorsal Central) Palatal       (Vowel Low)                _    ) = error "No palatal low vowels"
phoneToString (P _                  (Tongue _ Dorsal Central) Velar         (Vowel Low)                _    ) = error "No velar low vowels"
phoneToString (P _                  (Tongue _ Dorsal Lateral) _             (Vowel Low)                _    ) = error "No lateral vowels"
phoneToString (P Closed             _                         _             (Vowel _  )                _    ) = error "No ejective vowels"
phoneToString (P VoicedIngressive   _                         _             Click                      _    ) = error "No voiced ingressive click"
phoneToString (P VoicedIngressive   _                         _             NasalStop                  Nasal) = error "No voiced ingressive nasal"
phoneToString (P VoicedIngressive   _                         _             (Stop (Fricated _))        _    ) = error "No voiced ingressive affricates"
phoneToString (P VoicedIngressive   _                         _             (Fricative NonSibilant)    _    ) = error "No voiced ingressive fricative"
phoneToString (P VoicedIngressive   _                         _             Trill                      _    ) = error "No voiced ingressive trill"
phoneToString (P VoicedIngressive   _                         _             Tap                        _    ) = error "No voiced ingressive tap"
phoneToString (P VoicedIngressive   _                         _             Approximant                _    ) = error "No voiced ingressive approximant"
phoneToString (P _                  Inferiolabial             _             (Stop (Fricated Sibilant)) _    ) = error "No Labial Sibilant Affricate"
phoneToString (P _                  Inferiolabial             _             (Fricative Sibilant)       _    ) = error "No Labial Sibilant Fricative"
phoneToString (P _                  Inferiolabial             Superiolabial Tap                        _    ) = error "No Bilabial Tap"
phoneToString (P _                  Inferiolabial             Superiolabial Approximant                _    ) = error "No Bilabial Approximant"
phoneToString (P _                  Inferiolabial             Dental        Click                      _    ) = error "No labiodental Click"
phoneToString (P _                  Inferiolabial             Dental        (Stop Aspirated )          Oral ) = error "No labiodental Stop"
phoneToString (P _                  Inferiolabial             Dental        (Stop Tenuis    )          Oral ) = error "No labiodental Stop"
phoneToString (P _                  Inferiolabial             Dental        (Stop Unreleased)          Oral ) = error "No labiodental Stop"
phoneToString (P Voiced (Tongue _ Apical Central) Dental Trill _) = error "No dental trill"
phoneToString (P Voiced (Tongue _ Apical Central) Dental Tap   _) = error "No dental tap"
-- breathy h
phoneToString (P Breathy NoActive NoPassive (Fricative NonSibilant) Oral) = "ɦ"
-- recursive cases
phoneToString (P Voiced (Tongue r Dorsal Central) place (Vowel height) Nasal) = phoneToString (P Voiced (Tongue r Dorsal Central) place (Vowel height) Oral) ++ "̃"
phoneToString (P Voiceless (Tongue r Dorsal Central) place (Vowel height) n) = phoneToString (P Voiced (Tongue r Dorsal Central) place (Vowel height) n) ++ "̥"
phoneToString (P Creaky a p m n) = phoneToString (P Voiced a p m n) ++ "̰"
phoneToString (P VoicedIngressive (Tongue r Dorsal Central) place (Vowel height) n) = phoneToString (P Voiced (Tongue r Dorsal Central) place (Vowel height) n) ++ "↓"
phoneToString (P Voiceless a p (Stop Aspirated) n) = phoneToString (P Voiceless a p (Stop Tenuis) n) ++ "ʰ"
phoneToString (P g a p (Stop Unreleased) n) = phoneToString (P g a p (Stop Tenuis) n) ++ "̚"
phoneToString (P Closed a p m n) | isObstruent m = phoneToString (P Voiceless a p m n) ++ "ʼ"
                                 | otherwise     = error "No ejective non-obstruents"
phoneToString (P Voiceless a p m n) | not (isObstruent m) = phoneToString (P Voiced a p m n) ++ "̥"
phoneToString (P Breathy a p m n) | isObstruent m = phoneToString (P Voiced a p m n) ++ "ʱ"
                                  | otherwise     = phoneToString (P Voiced a p m n) ++ "̤"
-- Vowels
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Vowel High) Oral) = "i"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Vowel MidHigh) Oral) = "e"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Vowel Mid) Oral) = "ɛ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Vowel MidLow) Oral) = "æ"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Palatal (Vowel High) Oral) = "y"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Palatal (Vowel MidHigh) Oral) = "ø"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Palatal (Vowel Mid) Oral) = "œ"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Palatal (Vowel MidLow) Oral) = "ɶ"
phoneToString (P Voiced (Tongue _ Dorsal Central) PalatoVelar (Vowel High) Oral) = "ɨ"
phoneToString (P Voiced (Tongue _ Dorsal Central) PalatoVelar (Vowel MidHigh) Oral) = "ɵ"
phoneToString (P Voiced (Tongue _ Dorsal Central) PalatoVelar (Vowel Mid) Oral) = "ə"
phoneToString (P Voiced (Tongue _ Dorsal Central) PalatoVelar (Vowel MidLow) Oral) = "ɐ"
phoneToString (P Voiced (Tongue _ Dorsal Central) PalatoVelar (Vowel Low) Oral) = "a"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Vowel High) Oral) = "ɯ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Vowel MidHigh) Oral) = "ɤ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Vowel Mid) Oral) = "ʌ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Vowel MidLow) Oral) = "ɑ"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Velar (Vowel High) Oral) = "u"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Velar (Vowel MidHigh) Oral) = "o"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Velar (Vowel Mid) Oral) = "ɔ"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Velar (Vowel MidLow) Oral) = "ɒ"
phoneToString (P Voiced (Tongue _ Dorsal Central) _ (Vowel MidLow) Oral) = error "That's a weird vowel"
phoneToString (P _ _ _ (Vowel _) _) = error "No non-dorsal vowels"
-- Bilabials
phoneToString (P Voiced Inferiolabial Superiolabial Click Oral) = "ʘ̬" -- not sure about this one
phoneToString (P Voiceless Inferiolabial Superiolabial Click Oral) = "ʘ"
phoneToString (P Voiced Inferiolabial Superiolabial (Stop Tenuis) Oral) = "b"
phoneToString (P Voiced Inferiolabial Superiolabial (Stop Tenuis) Nasal) = "ᵐb"
phoneToString (P Voiceless Inferiolabial Superiolabial (Stop Tenuis) Oral) = "p"
phoneToString (P VoicedIngressive Inferiolabial Superiolabial (Stop Tenuis) Oral) = "ɓ"
phoneToString (P Voiced Inferiolabial Superiolabial NasalStop Nasal) = "m"
phoneToString (P Voiced Inferiolabial Superiolabial (Stop (Fricated NonSibilant)) Oral) = "b͡β"
phoneToString (P Voiceless Inferiolabial Superiolabial (Stop (Fricated NonSibilant)) Oral) = "p͡ɸ"
phoneToString (P Voiced Inferiolabial Superiolabial (Fricative NonSibilant) Oral) = "β"
phoneToString (P Voiceless Inferiolabial Superiolabial (Fricative NonSibilant) Oral) = "ɸ"
phoneToString (P Voiced Inferiolabial Superiolabial Trill Oral) = "ʙ"
-- Labiodentals
phoneToString (P voiced Inferiolabial Dental NasalStop Nasal) = "ɱ"
phoneToString (P Voiced Inferiolabial Dental (Stop (Fricated NonSibilant)) Oral) = "b͡v"
phoneToString (P Voiceless Inferiolabial Dental (Stop (Fricated NonSibilant)) Oral) = "p͡f"
phoneToString (P Voiced Inferiolabial Dental (Fricative NonSibilant) Oral) = "v"
phoneToString (P Voiceless Inferiolabial Dental (Fricative NonSibilant) Oral) = "f"
phoneToString (P _ Inferiolabial Dental Trill Oral) = error "No labiodental trill"
phoneToString (P Voiced Inferiolabial Dental Tap Oral) = "ⱱ"
phoneToString (P Voiced Inferiolabial Dental Approximant Oral) = "ʋ"
-- Dentals
phoneToString (P Voiced (Tongue _ Apical Central) Dental Click Oral) = "ǀ̬" -- not sure about this one
phoneToString (P Voiceless (Tongue _ Apical Central) Dental Click Oral) = "ǀ"
phoneToString (P Voiced (Tongue _ Apical Central) Dental (Stop Tenuis) Oral) = "d̪"
phoneToString (P Voiced (Tongue _ Apical Central) Dental (Stop Tenuis) Nasal) = "ⁿd̪"
phoneToString (P Voiceless (Tongue _ Apical Central) Dental (Stop Tenuis) Oral) = "t̪"
phoneToString (P VoicedIngressive (Tongue _ Apical Central) Dental (Stop Tenuis) Oral) = "ɗ̪"
phoneToString (P Voiced (Tongue _ Apical Central) Dental NasalStop Nasal) = "n̪"
phoneToString (P Voiced (Tongue _ Apical Central) Dental (Stop (Fricated NonSibilant)) Oral) = "d̪͡ð"
phoneToString (P Voiceless (Tongue _ Apical Central) Dental (Stop (Fricated NonSibilant)) Oral) = "t̪͡θ"
phoneToString (P Voiced (Tongue _ Apical Central) Dental (Fricative NonSibilant) Oral) = "ð"
phoneToString (P Voiceless (Tongue _ Apical Central) Dental (Fricative NonSibilant) Oral) = "θ"
phoneToString (P Voiced (Tongue _ Apical Central) Dental (Fricative Sibilant) Oral) = "z̪"
phoneToString (P Voiceless (Tongue _ Apical Central) Dental (Fricative Sibilant) Oral) = "s̪"
phoneToString (P Voiced (Tongue _ Laminal Central) Dental Trill Oral) = "r̪"
phoneToString (P Voiced (Tongue _ Apical Central) Dental Approximant Oral) = "ɹ̪"
phoneToString (P Voiced (Tongue _ Apical Lateral) Dental Approximant Oral) = "l̪"
-- Alveolars
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar Click Oral) = "ǃ̬"
phoneToString (P Voiceless (Tongue _ Apical Central) Alveolar Click Oral) = "ǃ"
phoneToString (P Voiced (Tongue _ Apical Lateral) Alveolar Click Oral) = "ǁ̬"
phoneToString (P Voiceless (Tongue _ Apical Lateral) Alveolar Click Oral) = "ǁ"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar (Stop Tenuis) Oral) = "d"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar (Stop Tenuis) Nasal) = "ⁿd"
phoneToString (P Voiceless (Tongue _ Apical Central) Alveolar (Stop Tenuis) Oral) = "t"
phoneToString (P VoicedIngressive (Tongue _ Apical Central) Alveolar (Stop Tenuis) Oral) = "ɗ"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar NasalStop Nasal) = "n"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar (Stop (Fricated Sibilant)) Oral) = "d͡z"
phoneToString (P Voiceless (Tongue _ Apical Central) Alveolar (Stop (Fricated Sibilant)) Oral) = "t͡s"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar (Stop (Fricated NonSibilant)) Oral) = "d͡ð͇"
phoneToString (P Voiceless (Tongue _ Apical Central) Alveolar (Stop (Fricated NonSibilant)) Oral) = "t͡θ͇"
phoneToString (P Voiced (Tongue _ Apical Lateral) Alveolar (Stop (Fricated NonSibilant)) Oral) = "d͡ɮ"
phoneToString (P Voiceless (Tongue _ Apical Lateral) Alveolar (Stop (Fricated NonSibilant)) Oral) = "t͡ɬ"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar (Fricative Sibilant) Oral) = "z"
phoneToString (P Voiceless (Tongue _ Apical Central) Alveolar (Fricative Sibilant) Oral) = "s"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar (Fricative NonSibilant) Oral) = "ð͇"
phoneToString (P Voiceless (Tongue _ Apical Central) Alveolar (Fricative NonSibilant) Oral) = "θ͇"
phoneToString (P Voiced (Tongue _ Apical Lateral) Alveolar (Fricative NonSibilant) Oral) = "ɮ"
phoneToString (P Voiceless (Tongue _ Apical Lateral) Alveolar (Fricative NonSibilant) Oral) = "ɬ"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar Trill Oral) = "r"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar Tap Oral) = "ɾ"
phoneToString (P Voiced (Tongue _ Apical Lateral) Alveolar Tap Oral) = "ɺ"
phoneToString (P Voiced (Tongue _ Apical Central) Alveolar Approximant Oral) = "ɹ"
phoneToString (P Voiced (Tongue _ Apical Lateral) Alveolar Approximant Oral) = "l"
-- Laminal Alveolars
phoneToString (P Voiced (Tongue _ Laminal Central) Alveolar Click Oral) = ""
-- Postalveolars
phoneToString (P Voiced (Tongue _ Apical Central) Postalveolar Click Oral) = "ǃ̬"
phoneToString (P Voiceless (Tongue _ Apical Central) Postalveolar Click Oral) = "ǃ"
phoneToString (P Voiced (Tongue _ Apical Central) Postalveolar (Stop (Fricated Sibilant)) Oral) = "d͡ʒ"
phoneToString (P Voiceless (Tongue _ Apical Central) Postalveolar (Stop (Fricated Sibilant)) Oral) = "t͡ʃ"
phoneToString (P Voiced (Tongue _ Apical Central) Postalveolar (Stop (Fricated NonSibilant)) Oral) = "d͡ɹ̠˔"
phoneToString (P Voiceless (Tongue _ Apical Central) Postalveolar (Stop (Fricated NonSibilant)) Oral) = "t͡ɹ̠̊˔"
phoneToString (P Voiced (Tongue _ Apical Central) Postalveolar (Fricative Sibilant) Oral) = "ʒ"
phoneToString (P Voiceless (Tongue _ Apical Central) Postalveolar (Fricative Sibilant) Oral) = "ʃ"
phoneToString (P Voiced (Tongue _ Apical Central) Postalveolar (Fricative NonSibilant) Oral) = "ɹ̠˔"
phoneToString (P Voiceless (Tongue _ Apical Central) Postalveolar (Fricative NonSibilant) Oral) = "ɹ̠̊˔"
phoneToString (P v (Tongue _ Apical l) Postalveolar m n) = phoneToString (P v (Tongue _ Apical l) Alveolar m n) ++ "̠"
-- palatoalveolars
phoneToString (P Voiced (Tongue _ Laminal Central) Postalveolar Click Oral) = "ǂ̬"
phoneToString (P Voiceless (Tongue _ Laminal Central) Postalveolar Click Oral) = "ǂ"
phoneToString (P Voiced (Tongue _ Laminal Central) Postalveolar (Stop (Fricated Sibilant)) Oral) = "d͡ʑ"
phoneToString (P Voiceless (Tongue _ Laminal Central) Postalveolar (Stop (Fricated Sibilant)) Oral) = "t͡ɕ"
phoneToString (P Voiced (Tongue _ Laminal Central) Postalveolar (Fricative Sibilant) Oral) = "ʑ"
phoneToString (P Voiceless (Tongue _ Laminal Central) Postalveolar (Fricative Sibilant) Oral) = "ɕ"
-- Retroflexes
phoneToString (P Voiced (Tongue _ Apical Central) Palatal Click Oral) = "‼̬"
phoneToString (P Voiceless (Tongue _ Apical Central) Palatal Click Oral) = "‼"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal (Stop Tenuis) Oral) = "ɖ"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal (Stop Tenuis) Nasal) = "ɳ͡ɖ"
phoneToString (P Voiceless (Tongue _ Apical Central) Palatal (Stop Tenuis) Oral) = "ʈ"
phoneToString (P VoicedIngressive (Tongue _ Apical Central) Palatal (Stop Tenuis) _) = error "No voiced ingressive retroflex stop"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal NasalStop Nasal) = "ɳ"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal (Stop (Fricated Sibilant)) Oral) = "ɖ͡ʐ"
phoneToString (P Voiceless (Tongue _ Apical Central) Palatal (Stop (Fricated Sibilant)) Oral) = "ʈ͡ʂ"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal (Stop (Fricated NonSibilant)) Oral) = "ɖ͡ɻ˔"
phoneToString (P Voiceless (Tongue _ Apical Central) Palatal (Stop (Fricated NonSibilant)) Oral) = "ʈ͡ɻ̊˔"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal (Fricative Sibilant) Oral) = "ʐ"
phoneToString (P Voiceless (Tongue _ Apical Central) Palatal (Fricative Sibilant) Oral) = "ʂ"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal (Fricative NonSibilant) Oral) = "ɻ˔"
phoneToString (P Voiceless (Tongue _ Apical Central) Palatal (Fricative NonSibilant) Oral) = "ɻ̊˔"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal Trill _) = error "No retroflex trill"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal Tap Oral) = "ɽ"
phoneToString (P Voiced (Tongue _ Apical Central) Palatal Approximant Oral) = "ɻ"
phoneToString (P Voiced (Tongue _ Apical Lateral) Palatal Approximant Oral) = "ɭ"
-- Palatals
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Stop Tenuis) Oral) = "ɟ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Stop Tenuis) Nasal) = "ɲ͡ɟ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Palatal (Stop Tenuis) Oral) = "c"
phoneToString (P VoicedIngressive (Tongue Unrounded Dorsal Central) Palatal (Stop Tenuis) Oral) = "ʄ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal NasalStop Nasal) = "ɲ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Stop (Fricated NonSibilant)) Oral) = "ɟ͡ʝ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Palatal (Stop (Fricated NonSibilant)) Oral) = "c͡ç"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal (Fricative NonSibilant) Oral) = "ʝ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Palatal (Fricative NonSibilant) Oral) = "ç"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Palatal Approximant Oral) = "j"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Palatal Approximant Oral) = "ɥ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Lateral) Palatal Approximant Oral) = "ʎ"
-- Velars
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Stop Tenuis) Oral) = "ɡ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Stop Tenuis) Nasal) = "ŋ͡ɡ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Velar (Stop Tenuis) Oral) = "k"
phoneToString (P VoicedIngressive (Tongue Unrounded Dorsal Central) Velar (Stop Tenuis) Oral) = "ɠ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar NasalStop Nasal) = "ŋ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Stop (Fricated NonSibilant)) Oral) = "ɡ͡ɣ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Velar (Stop (Fricated NonSibilant)) Oral) = "k͡x"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar (Fricative NonSibilant) Oral) = "ɣ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Velar (Fricative NonSibilant) Oral) = "x"
phoneToString (P Voiceless (Tongue Rounded Dorsal Central) Velar (Fricative NonSibilant) Oral) = "ʍ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Velar Approximant Oral) = "ɰ"
phoneToString (P Voiced (Tongue Rounded Dorsal Central) Velar Approximant Oral) = "w"
phoneToString (P Voiced (Tongue Unrounded Dorsal Lateral) Velar Approximant Oral) = "ʟ"
-- Uvulars
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Uvular (Stop Tenuis) Oral) = "ɢ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Uvular (Stop Tenuis) Nasal) = "ɴ͡ɢ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Uvular (Stop Tenuis) Oral) = "q"
phoneToString (P VoicedIngressive (Tongue Unrounded Dorsal Central) Uvular (Stop Tenuis) Oral) = "ʛ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Uvular NasalStop Nasal) = "ɴ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Uvular (Stop (Fricated NonSibilant)) Oral) = "ɢ͡ʁ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Uvular (Stop (Fricated NonSibilant)) Oral) = "q͡χ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Uvular (Fricative NonSibilant) Oral) = "ʁ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Uvular (Fricative NonSibilant) Oral) = "χ"
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Uvular Trill Oral) = "ʀ"
-- Pharyngeals
phoneToString (P Voiced (Tongue Unrounded Dorsal Central) Pharyngeal (Fricative NonSibilant) Oral) = "ʕ"
phoneToString (P Voiceless (Tongue Unrounded Dorsal Central) Pharyngeal (Fricative NonSibilant) Oral) = "ħ"
-- Epiglottals
phoneToString (P Voiceless Epiglottal Pharyngeal (Stop Tenuis) Oral) = "ʡ"
phoneToString (P Voiced Epiglottal Pharyngeal (Fricative NonSibilant) Oral) = "ʢ"
phoneToString (P Voiceless Epiglottal Pharyngeal (Fricative NonSibilant) Oral) = "ʜ"
-- Glottals
phoneToString (P Voiceless NoActive NoPassive (Stop Tenuis) Oral) = "ʔ"
phoneToString (P Voiceless NoActive NoPassive (Fricative NonSibilant) Oral) = "h"
-- ono :(
phoneToString (P {}) = error "AAAAH!!! I missed a case!"

a :: [(Int,Bool)]
a = [ (p,x == [y]) | (p,(x,y)) <- zip [0..] (zip (map (phoneToString . charToPhone) universe) universe)]