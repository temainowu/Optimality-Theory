module OptimalityTheory.Phones where

data Phone = P GlottalState Active Passive Manner Nasality | SyllableBoundary | MorphemeBoundary | WordBoundary
    deriving (Eq)
    
instance Show Phone where
    show = phoneToString

data Passive = Superiolabial | Dental | Alveolar | Postalveolar | Palatal | Central | Velar | Uvular | Pharyngeal | NoPassive
    deriving (Eq, Show)

instance Enum Passive where
    fromEnum Superiolabial = 0
    fromEnum Dental = 1
    fromEnum Alveolar = 2
    fromEnum Postalveolar = 3
    fromEnum Palatal = 4
    fromEnum Central = 5
    fromEnum Velar = 6
    fromEnum Uvular = 7
    fromEnum Pharyngeal = 8
    fromEnum NoPassive = 9
    toEnum _ = NoPassive

data Active = Inferiolabial | Tongue TonguePlace Laterality | Epiglottal | NoActive
    deriving (Eq, Show)

instance Enum Active where
    fromEnum Inferiolabial = 0
    fromEnum (Tongue Apical _) = 1
    fromEnum (Tongue Laminal _) = 2
    fromEnum (Tongue (Dorsal _) _) = 3
    fromEnum Epiglottal = 4
    fromEnum NoActive = 5
    toEnum _ = NoActive


data TonguePlace = Apical | Laminal | Dorsal Rounding
    deriving (Eq, Show)

data Manner = Click | Stop Release | Fricative Sibilance | NasalStop | Trill | Tap | Approximant | Vowel Height
    deriving (Eq, Show)

data Height = High | MidHigh | Mid | MidLow | Low
    deriving (Eq, Show)

data Sibilance = Sibilant | NonSibilant
    deriving (Eq, Show)

data Laterality = Lateral | NonLateral
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
rounded = "ʍwɥyøœɵəɶɐaʊuoɔɒ"
sib = "szʃʒɕʑʂʐ"

-- glottal states
unvoiced = "pɸfθtsɬʃʈʂcçkxʍqχħhʜʢʡʘǀǃ‼ǂǁ"
voiced = "bmʙβɱⱱvʋðdnrɾɺzɮɹlʒɕʑɖɳɽʐɻɭɟɲʝjɥʎgŋɣɰwʟɢɴʀʁʕieɛæɶɪyøœɨɵəɐaɯɤʌɑʊuoɔɒ"
creaky = ""
breathy = "ɦ"
closed = "ʔ"
ingressive = "ɓɗʄɠʛ"
asp = ""

-- active articulators
inflab = "pbmʙɸβɱⱱfvʋʘɓ"
api = "θðtdnrɾɺszɬɮɹlʃʒʈɖɳɽʂʐɻɭǀǃ‼ǁɗ"
lam = "ɕʑǂ"
dors = "cɟɲçʝjɥʎkgŋxʍɣɰwʟqɢɴʀχʁħʕʄɠʛieɛæɪyøœɶɨɵəɐaɯɤʌɑʊuoɔɒ"
epi = "ʜʢʡ"

-- passive articulators
suplab = "pbmʙɸβʘɓ"
dent = "ɱⱱfvʋθðǀ"
alv = "tdnrɾɺszɬɮɹlǁɗ"
postalv = "ʃʒɕʑǃǂ"
pal = "ʈɖɳɽʂʐɻɭcɟɲçʝjɥʎ‼ʄieɛæɶɪyøœ"
cent = "ɨɵəɐa"
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
hi = "iɪyɨɯʊu"
mhi = "eøɵɤo"
mid = "ɛœəʌɔ"
mlo = "æɶɐɑɒ"
lo = "a"

isObstruent :: Manner -> Bool
isObstruent m = m `elem` [Click, Stop Tenuis, Stop Aspirated, Stop Unreleased, Fricative Sibilant, Fricative NonSibilant, Stop (Fricated Sibilant), Stop (Fricated NonSibilant)]

passiveOf :: Char -> Passive
passiveOf x
    | x `elem` suplab = Superiolabial
    | x `elem` dent = Dental
    | x `elem` alv = Alveolar
    | x `elem` postalv = Postalveolar
    | x `elem` pal = Palatal
    | x `elem` cent = Central
    | x `elem` vel = Velar
    | x `elem` uvul = Uvular
    | x `elem` phar = Pharyngeal
    | otherwise = NoPassive

activeOf :: Char -> Active
activeOf x
    | x `elem` inflab = Inferiolabial
    | x `elem` lat = case () of
         () | x `elem` api -> Tongue Apical Lateral
            | x `elem` lam -> Tongue Laminal Lateral
            | x `elem` dors && x `notElem` rounded -> Tongue (Dorsal Unrounded) Lateral
            | x `elem` dors && x `elem` rounded -> Tongue (Dorsal Rounded) Lateral
    | x `elem` api && x `notElem` lat = Tongue Apical NonLateral
    | x `elem` lam && x `notElem` lat = Tongue Laminal NonLateral
    | x `elem` dors && x `notElem` rounded = Tongue (Dorsal Unrounded) NonLateral
    | x `elem` dors && x `elem` rounded = Tongue (Dorsal Rounded) NonLateral
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
phoneToString (P _                  (Tongue (Dorsal _) NonLateral) Palatal (Vowel Low)              _) = error "No palatal low vowels"
phoneToString (P _                  (Tongue (Dorsal _) NonLateral) Velar   (Vowel Low)              _) = error "No velar low vowels"
phoneToString (P _                  (Tongue (Dorsal _) Lateral)    _       (Vowel Low)              _) = error "No lateral vowels"
phoneToString (P Closed             _                              _       (Vowel _  )              _) = error "No ejective vowels"
phoneToString (P VoicedIngressive   _                              _       Click                    _) = error "No voiced ingressive click"
phoneToString (P VoicedIngressive   _                              _       NasalStop            Nasal) = error "No voiced ingressive nasal"
phoneToString (P VoicedIngressive   _                              _       (Stop (Fricated _))      _) = error "No voiced ingressive affricates"
phoneToString (P VoicedIngressive   _                              _       (Fricative NonSibilant)  _) = error "No voiced ingressive fricative"
phoneToString (P VoicedIngressive   _                              _       Trill                    _) = error "No voiced ingressive trill"
phoneToString (P VoicedIngressive   _                              _       Tap                      _) = error "No voiced ingressive tap"
phoneToString (P VoicedIngressive   _                              _       Approximant              _) = error "No voiced ingressive approximant"
phoneToString (P _                  Inferiolabial                  _       (Stop (Fricated Sibilant)) _) = error "No Labial Sibilant Affricate"
phoneToString (P _                  Inferiolabial                  _       (Fricative Sibilant)     _) = error "No Labial Sibilant Fricative"
phoneToString (P _                  Inferiolabial                  Superiolabial Tap                _) = error "No Bilabial Tap"
phoneToString (P _                  Inferiolabial                  Superiolabial Approximant        _) = error "No Bilabial Approximant"
phoneToString (P _                  Inferiolabial                  Dental        Click              _) = error "No labiodental Click"
phoneToString (P _                  Inferiolabial                  Dental        (Stop _)        Oral) = error "No labiodental Stop"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Trill _) = error "No dental trill"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Tap   _) = error "No dental tap"
-- breathy h
phoneToString (P Breathy NoActive NoPassive (Fricative NonSibilant) Oral) = "ɦ"
-- recursive cases
phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height) Nasal) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height) Oral) ++ "̃"
phoneToString (P Voiceless (Tongue (Dorsal rounding) NonLateral) place (Vowel height) n) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height) n) ++ "̥"
phoneToString (P Creaky a p m n) = phoneToString (P Voiced a p m n) ++ "̰"
phoneToString (P VoicedIngressive (Tongue (Dorsal rounding) NonLateral) place (Vowel height) n) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height) n) ++ "↓"
phoneToString (P Voiceless a p (Stop Aspirated) n) = phoneToString (P Voiceless a p (Stop Tenuis) n) ++ "ʰ"
phoneToString (P g a p (Stop Unreleased) n) = phoneToString (P g a p (Stop Tenuis) n) ++ "̚"
phoneToString (P Closed a p m n) | isObstruent m = phoneToString (P Voiceless a p m n) ++ "ʼ"
                                 | otherwise     = error "No ejective non-obstruents"
phoneToString (P Voiceless a p m n) | not (isObstruent m) = phoneToString (P Voiced a p m n) ++ "̥"
phoneToString (P Breathy a p m n) | isObstruent m = phoneToString (P Voiced a p m n) ++ "ʱ"
                                  | otherwise     = phoneToString (P Voiced a p m n) ++ "̤"
-- Vowels
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel High) Oral) = "i"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel MidHigh) Oral) = "e"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel Mid) Oral) = "ɛ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel MidLow) Oral) = "æ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel High) Oral) = "y"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel MidHigh) Oral) = "ø"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel Mid) Oral) = "œ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel MidLow) Oral) = "ɶ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel High) Oral) = "ɨ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel MidHigh) Oral) = "ɵ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel Mid) Oral) = "ə"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel MidLow) Oral) = "ɐ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel Low) Oral) = "a"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel High) Oral) = "ɯ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel MidHigh) Oral) = "ɤ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel Mid) Oral) = "ʌ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel MidLow) Oral) = "ɑ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel High) Oral) = "u"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel MidHigh) Oral) = "o"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel Mid) Oral) = "ɔ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel MidLow) Oral) = "ɒ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) _ (Vowel MidLow) Oral) = error "That's a weird vowel"
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
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Click Oral) = "ǀ̬" -- not sure about this one
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental Click Oral) = "ǀ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Stop Tenuis) Oral) = "d̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Stop Tenuis) Nasal) = "ⁿd̪"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Stop Tenuis) Oral) = "t̪"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Dental (Stop Tenuis) Oral) = "ɗ̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental NasalStop Nasal) = "n̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Stop (Fricated NonSibilant)) Oral) = "d̪͡ð"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Stop (Fricated NonSibilant)) Oral) = "t̪͡θ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Fricative NonSibilant) Oral) = "ð"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Fricative NonSibilant) Oral) = "θ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Fricative Sibilant) Oral) = "z̪"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Fricative Sibilant) Oral) = "s̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Trill Oral) = "r̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Approximant Oral) = "ɹ̪"
phoneToString (P Voiced (Tongue Apical Lateral) Dental Approximant Oral) = "l̪"
-- Alveolars
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Click Oral) = "ǃ̬"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar Click Oral) = "ǃ"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar Click Oral) = "ǁ̬"
phoneToString (P Voiceless (Tongue Apical Lateral) Alveolar Click Oral) = "ǁ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Stop Tenuis) Oral) = "d"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Stop Tenuis) Nasal) = "ⁿd"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Stop Tenuis) Oral) = "t"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Alveolar (Stop Tenuis) Oral) = "ɗ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar NasalStop Nasal) = "n"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Stop (Fricated Sibilant)) Oral) = "d͡z"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Stop (Fricated Sibilant)) Oral) = "t͡s"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Stop (Fricated NonSibilant)) Oral) = "d͡ð͇"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Stop (Fricated NonSibilant)) Oral) = "t͡θ͇"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar (Stop (Fricated NonSibilant)) Oral) = "d͡ɮ"
phoneToString (P Voiceless (Tongue Apical Lateral) Alveolar (Stop (Fricated NonSibilant)) Oral) = "t͡ɬ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Fricative Sibilant) Oral) = "z"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Fricative Sibilant) Oral) = "s"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Fricative NonSibilant) Oral) = "ð͇"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Fricative NonSibilant) Oral) = "θ͇"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar (Fricative NonSibilant) Oral) = "ɮ"
phoneToString (P Voiceless (Tongue Apical Lateral) Alveolar (Fricative NonSibilant) Oral) = "ɬ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Trill Oral) = "r"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Tap Oral) = "ɾ"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar Tap Oral) = "ɺ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Approximant Oral) = "ɹ"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar Approximant Oral) = "l"
-- Laminal Alveolars
phoneToString (P Voiced (Tongue Laminal NonLateral) Alveolar Click Oral) = ""
-- Postalveolars
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Click Oral) = "ǃ̬"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar Click Oral) = "ǃ"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Stop Tenuis) Oral) = "d̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Stop Tenuis) Nasal) = "ⁿd̠"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Stop Tenuis) Oral) = "t̠"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Postalveolar (Stop Tenuis) Oral) = "ɗ̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar NasalStop Nasal) = "n̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Stop (Fricated Sibilant)) Oral) = "d͡ʒ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Stop (Fricated Sibilant)) Oral) = "t͡ʃ"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Stop (Fricated NonSibilant)) Oral) = "d͡ɹ̠˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Stop (Fricated NonSibilant)) Oral) = "t͡ɹ̠̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Fricative Sibilant) Oral) = "ʒ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Fricative Sibilant) Oral) = "ʃ"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Fricative NonSibilant) Oral) = "ɹ̠˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Fricative NonSibilant) Oral) = "ɹ̠̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Trill Oral) = "r̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Tap Oral) = "ɾ̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Approximant Oral) = "ɹ̠"
phoneToString (P Voiced (Tongue Apical Lateral) Postalveolar Approximant Oral) = "l̠"
-- palatoalveolars
phoneToString (P Voiced (Tongue Laminal NonLateral) Postalveolar Click Oral) = "ǂ̬"
phoneToString (P Voiceless (Tongue Laminal NonLateral) Postalveolar Click Oral) = "ǂ"
phoneToString (P Voiced (Tongue Laminal NonLateral) Postalveolar (Stop (Fricated Sibilant)) Oral) = "d͡ʑ"
phoneToString (P Voiceless (Tongue Laminal NonLateral) Postalveolar (Stop (Fricated Sibilant)) Oral) = "t͡ɕ"
phoneToString (P Voiced (Tongue Laminal NonLateral) Postalveolar (Fricative Sibilant) Oral) = "ʑ"
phoneToString (P Voiceless (Tongue Laminal NonLateral) Postalveolar (Fricative Sibilant) Oral) = "ɕ"
-- Retroflexes
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal Click Oral) = "‼̬"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal Click Oral) = "‼"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Stop Tenuis) Oral) = "ɖ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Stop Tenuis) Nasal) = "ɳ͡ɖ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Stop Tenuis) Oral) = "ʈ"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Palatal (Stop Tenuis) _) = error "No voiced ingressive retroflex stop"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal NasalStop Nasal) = "ɳ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Stop (Fricated Sibilant)) Oral) = "ɖ͡ʐ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Stop (Fricated Sibilant)) Oral) = "ʈ͡ʂ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Stop (Fricated NonSibilant)) Oral) = "ɖ͡ɻ˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Stop (Fricated NonSibilant)) Oral) = "ʈ͡ɻ̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Fricative Sibilant) Oral) = "ʐ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Fricative Sibilant) Oral) = "ʂ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Fricative NonSibilant) Oral) = "ɻ˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Fricative NonSibilant) Oral) = "ɻ̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal Trill _) = error "No retroflex trill"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal Tap Oral) = "ɽ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal Approximant Oral) = "ɻ"
phoneToString (P Voiced (Tongue Apical Lateral) Palatal Approximant Oral) = "ɭ"
-- Palatals
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Tenuis) Oral) = "ɟ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Tenuis) Nasal) = "ɲ͡ɟ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Tenuis) Oral) = "c"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Tenuis) Oral) = "ʄ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal NasalStop Nasal) = "ɲ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop (Fricated NonSibilant)) Oral) = "ɟ͡ʝ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop (Fricated NonSibilant)) Oral) = "c͡ç"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Fricative NonSibilant) Oral) = "ʝ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Palatal (Fricative NonSibilant) Oral) = "ç"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal Approximant Oral) = "j"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal Approximant Oral) = "ɥ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) Lateral) Palatal Approximant Oral) = "ʎ"
-- Velars
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Tenuis) Oral) = "ɡ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Tenuis) Nasal) = "ŋ͡ɡ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Tenuis) Oral) = "k"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Tenuis) Oral) = "ɠ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar NasalStop Nasal) = "ŋ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop (Fricated NonSibilant)) Oral) = "ɡ͡ɣ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop (Fricated NonSibilant)) Oral) = "k͡x"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Fricative NonSibilant) Oral) = "ɣ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Velar (Fricative NonSibilant) Oral) = "x"
phoneToString (P Voiceless (Tongue (Dorsal Rounded) NonLateral) Velar (Fricative NonSibilant) Oral) = "ʍ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar Approximant Oral) = "ɰ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar Approximant Oral) = "w"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) Lateral) Velar Approximant Oral) = "ʟ"
-- Uvulars
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Tenuis) Oral) = "ɢ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Tenuis) Nasal) = "ɴ͡ɢ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Tenuis) Oral) = "q"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Tenuis) Oral) = "ʛ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular NasalStop Nasal) = "ɴ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop (Fricated NonSibilant)) Oral) = "ɢ͡ʁ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop (Fricated NonSibilant)) Oral) = "q͡χ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Fricative NonSibilant) Oral) = "ʁ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Uvular (Fricative NonSibilant) Oral) = "χ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular Trill Oral) = "ʀ"
-- Pharyngeals
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Pharyngeal (Fricative NonSibilant) Oral) = "ʕ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Pharyngeal (Fricative NonSibilant) Oral) = "ħ"
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