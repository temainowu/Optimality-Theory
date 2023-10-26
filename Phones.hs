module OptimalityTheory.Phones where

data Phone = P GlottalState Active Passive Manner | SyllableBoundary | MorphemeBoundary | WordBoundary
    deriving (Eq, Show)

data Passive = Superiolabial | Dental | Alveolar | Postalveolar | Palatal | Central | Velar | Uvular | Pharyngeal | NoPassive
    deriving (Eq, Show)

data Active = Inferiolabial | Tongue TonguePlace Laterality | Epiglottal | NoActive
    deriving (Eq, Show)

data TonguePlace = Apical | Laminal | Dorsal Rounding
    deriving (Eq, Show)

data Manner = Click | Stop Nasality | Affricate Sibilance | Fricative Sibilance | Trill | Tap | Approximant | Vowel Height Nasality
    deriving (Eq, Show)

data Height = High | MidHigh | Mid | MidLow | Low
    deriving (Eq, Show)

data Sibilance = Sibilant | NonSibilant
    deriving (Eq, Show)

data Laterality = Lateral | NonLateral
    deriving (Eq, Show)

data GlottalState = Voiced | Voiceless | Creaky | Breathy | Closed | VoicedIngressive | VoicelessAspirated
    deriving (Eq, Show)

data Rounding = Rounded | Unrounded
    deriving (Eq, Show)

data Nasality = Nasal | Oral
    deriving (Eq, Show)

universe :: String
universe = "pbmʙɸβɱⱱfvʋθðtdnrɾɺszɬɮɹlʃʒɕʑʈɖɳɽʂʐɻɭcɟɲçʝjɥʎkgŋxʍɣɰwʟqɢɴʀχʁħʕhɦʔʜʢʡʘǀǃǂǁɓɗʄɠʛieɛæɶɪyøœɵəɐaɯɤʌɑʊuoɔɒ"
lat = "ɺɬɮlɭʎʟǁ"
rounded = "ʍwɥyøœɵəɶɐaʊuoɔɒ"
sib = "szʃʒɕʑʂʐ"

-- glottal states
unvoiced = "pɸfθtsɬʃʈʂcçkxʍqχħhʜʢʡʘǀǃǂǁ"
voiced = "bmʙβɱⱱvʋðdnrɾɺzɮɹlʒɕʑɖɳɽʐɻɭɟɲʝjɥʎgŋɣɰwʟɢɴʀʁʕieɛæɶɪyøœɨɵəɐaɯɤʌɑʊuoɔɒ"
creaky = ""
breathy = "ɦ"
closed = "ʔ"
ingressive = "ɓɗʄɠʛ"
asp = ""

-- active articulators
inflab = "pbmʙɸβɱⱱfvʋʘɓ"
api = "θðtdnrɾɺszɬɮɹlʃʒʈɖɳɽʂʐɻɭǀǃǁɗ"
lam = "ɕʑǂ"
dors = "cɟɲçʝjɥʎkgŋxʍɣɰwʟqɢɴʀχʁħʕʄɠʛieɛæɪyøœɶɨɵəɐaɯɤʌɑʊuoɔɒ"
epi = "ʜʢʡ"

-- passive articulators
suplab = "pbmʙɸβʘɓ"
dent = "ɱⱱfvʋθðǀ"
alv = "tdnrɾɺszɬɮɹlǁɗ"
postalv = "ʃʒɕʑǃǂ"
pal = "ʈɖɳɽʂʐɻɭcɟɲçʝjɥʎʄieɛæɶɪyøœ"
cent = "ɨɵəɐa"
vel = "kgŋxʍɣɰwʟɠɯɤʌɑʊuoɔɒ"
uvul = "qɢɴʀχʁʛ"
phar = "ħʕʜʢʡ"

-- manners
click = "ʘǀǃǂǁ"
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
isObstruent m = m `elem` [Click, Stop Oral, Fricative Sibilant, Fricative NonSibilant, Affricate Sibilant, Affricate NonSibilant]

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
    | x `elem` asp = VoicelessAspirated

mannerOf :: Char -> Manner
mannerOf x
    | x `elem` click = Click
    | x `elem` stop = Stop Oral
    | x `elem` fric && x `elem` sib = Fricative Sibilant
    | x `elem` fric && x `notElem` sib = Fricative NonSibilant
    | x `elem` nas = Stop Nasal
    | x `elem` trill = Trill
    | x `elem` tap = Tap
    | x `elem` appr = Approximant
    | x `elem` hi && x `elem` nas = Vowel High Nasal
    | x `elem` hi = Vowel High Oral
    | x `elem` mhi && x `elem` nas = Vowel MidHigh Nasal
    | x `elem` mhi = Vowel MidHigh Oral
    | x `elem` mid && x `elem` nas = Vowel Mid Nasal
    | x `elem` mid = Vowel Mid Oral
    | x `elem` mlo && x `elem` nas = Vowel MidLow Nasal
    | x `elem` mlo = Vowel MidLow Oral
    | x `elem` lo && x `elem` nas = Vowel Low Nasal
    | x `elem` lo = Vowel Low Oral

charToPhone :: Char -> Phone
charToPhone '.' = SyllableBoundary
charToPhone '+' = MorphemeBoundary
charToPhone '#' = WordBoundary
charToPhone x = P (glottalStateOf x) (activeOf x) (passiveOf x) (mannerOf x)


phoneToString :: Phone -> String
phoneToString WordBoundary = "#"
phoneToString MorphemeBoundary = "+"
phoneToString SyllableBoundary = "."
-- Errors
phoneToString (P _                  (Tongue (Dorsal _) NonLateral) Palatal (Vowel Low _))        = error "No palatal low vowels"
phoneToString (P _                  (Tongue (Dorsal _) NonLateral) Velar   (Vowel Low _))        = error "No velar low vowels"
phoneToString (P _                  (Tongue (Dorsal _) Lateral)    _       (Vowel Low _))        = error "No lateral vowels"
phoneToString (P Closed             _                              _       (Vowel _   _))        = error "No ejective vowels"
phoneToString (P VoicelessAspirated _                              _       (Vowel _   _))        = error "No aspirated vowels"
phoneToString (P VoicedIngressive   _                              _       m)                    | m /= Stop Oral = error "only plosives can be voiced ingressive"
phoneToString (P VoicedIngressive   _                              _       Click)                = error "No voiced ingressive click"
phoneToString (P VoicedIngressive   _                              _       (Stop Nasal))         = error "No voiced ingressive nasal"
phoneToString (P VoicedIngressive   _                              _       (Affricate _))        = error "No voiced ingressive affricates"
phoneToString (P VoicedIngressive   _                              _       (Fricative NonSibilant)) = error "No voiced ingressive fricative"
phoneToString (P VoicedIngressive   _                              _       Trill)                   = error "No voiced ingressive trill"
phoneToString (P VoicedIngressive   _                              _       Tap)                     = error "No voiced ingressive tap"
phoneToString (P VoicedIngressive   _                              _       Approximant)             = error "No voiced ingressive approximant"
phoneToString (P _                  Inferiolabial                  _       (Affricate Sibilant))    = error "No Labial Sibilant Affricate"
phoneToString (P _                  Inferiolabial                  _       (Fricative Sibilant))    = error "No Labial Sibilant Fricative"
phoneToString (P _                  Inferiolabial                  Superiolabial Tap)               = error "No Bilabial Tap"
phoneToString (P _                  Inferiolabial                  Superiolabial Approximant)       = error "No Bilabial Approximant"
phoneToString (P _                  Inferiolabial                  Dental        Click)             = error "No labiodental Click"
phoneToString (P _                  Inferiolabial                  Dental        (Stop Oral))       = error "No labiodental Stop"

phoneToString (P Voiced (Tongue Apical NonLateral) Dental Trill) = error "No dental trill"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Tap) = error "No dental tap"
-- breathy h
phoneToString (P Breathy NoActive NoPassive (Fricative NonSibilant)) = "ɦ"
-- recursive cases
phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height Nasal)) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height Oral)) ++ "̃"
phoneToString (P Voiceless (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) ++ "̥"
phoneToString (P Creaky a p m) = phoneToString (P Voiced a p m) ++ "̰"
phoneToString (P VoicedIngressive (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) ++ "↓"
phoneToString (P VoicelessAspirated a p m) = phoneToString (P Voiceless a p m) ++ "ʰ"
phoneToString (P Closed a p m) | isObstruent m = phoneToString (P Voiceless a p m) ++ "ʼ"
                               | otherwise = error "No ejective non-obstruents"
phoneToString (P Voiceless a p m) | not (isObstruent m) = phoneToString (P Voiced a p m) ++ "̥"
phoneToString (P Breathy a p m) | isObstruent m = phoneToString (P Voiced a p m) ++ "ʱ"
                                | otherwise = phoneToString (P Voiced a p m) ++ "̤"
-- Vowels
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel High Oral)) = "i"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel MidHigh Oral)) = "e"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel Mid Oral)) = "ɛ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel MidLow Oral)) = "æ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel High Oral)) = "y"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel MidHigh Oral)) = "ø"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel Mid Oral)) = "œ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal (Vowel MidLow Oral)) = "ɶ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel High Oral)) = "ɨ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel MidHigh Oral)) = "ɵ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel Mid Oral)) = "ə"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel MidLow Oral)) = "ɐ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) Central (Vowel Low Oral)) = "a"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel High Oral)) = "ɯ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel MidHigh Oral)) = "ɤ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel Mid Oral)) = "ʌ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Vowel MidLow Oral)) = "ɑ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel High Oral)) = "u"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel MidHigh Oral)) = "o"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel Mid Oral)) = "ɔ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar (Vowel MidLow Oral)) = "ɒ"
phoneToString (P Voiced (Tongue (Dorsal _) NonLateral) _ (Vowel MidLow Oral)) = error "That's a weird vowel"
phoneToString (P _ _ _ (Vowel _ _)) = error "No non-dorsal vowels"
-- Bilabials
phoneToString (P Voiced Inferiolabial Superiolabial Click) = "ʘ̬" -- not sure about this one
phoneToString (P Voiceless Inferiolabial Superiolabial Click) = "ʘ"
phoneToString (P Voiced Inferiolabial Superiolabial (Stop Oral)) = "b"
phoneToString (P Voiceless Inferiolabial Superiolabial (Stop Oral)) = "p"
phoneToString (P VoicedIngressive Inferiolabial Superiolabial (Stop Oral)) = "ɓ"
phoneToString (P Voiced Inferiolabial Superiolabial (Stop Nasal)) = "m"
phoneToString (P VoicedIngressive Inferiolabial Superiolabial (Stop Nasal)) = "ɓ̃" -- not sure about this one
phoneToString (P Voiced Inferiolabial Superiolabial (Affricate NonSibilant)) = "b͡β"
phoneToString (P Voiceless Inferiolabial Superiolabial (Affricate NonSibilant)) = "p͡ɸ"
phoneToString (P Voiced Inferiolabial Superiolabial (Fricative NonSibilant)) = "β"
phoneToString (P Voiceless Inferiolabial Superiolabial (Fricative NonSibilant)) = "ɸ"
phoneToString (P Voiced Inferiolabial Superiolabial Trill) = "ʙ"
-- Labiodentals
phoneToString (P voiced Inferiolabial Dental (Stop Nasal)) = "ɱ"
phoneToString (P Voiced Inferiolabial Dental (Affricate NonSibilant)) = "b͡v"
phoneToString (P Voiceless Inferiolabial Dental (Affricate NonSibilant)) = "p͡f"
phoneToString (P Voiced Inferiolabial Dental (Fricative NonSibilant)) = "v"
phoneToString (P Voiceless Inferiolabial Dental (Fricative NonSibilant)) = "f"
phoneToString (P _ Inferiolabial Dental Trill) = error "No labiodental trill"
phoneToString (P Voiced Inferiolabial Dental Tap) = "ⱱ"
phoneToString (P Voiced Inferiolabial Dental Approximant) = "ʋ"
-- Dentals
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Click) = "ǀ̬" -- not sure about this one
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental Click) = "ǀ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Stop Oral)) = "d̪"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Stop Oral)) = "t̪"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Dental (Stop Oral)) = "ɗ̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Stop Nasal)) = "n̪"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Dental (Stop Nasal)) = "ɗ̪̃" -- not sure about this one
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Affricate NonSibilant)) = "d̪͡ð"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Affricate NonSibilant)) = "t̪͡θ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Fricative NonSibilant)) = "ð"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Fricative NonSibilant)) = "θ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Fricative Sibilant)) = "z̪"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Fricative Sibilant)) = "s̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Trill) = "r̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Approximant) = "ɹ̪"
phoneToString (P Voiced (Tongue Apical Lateral) Dental Approximant) = "l̪"
-- Alveolars
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Click) = "ǃ̬"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar Click) = "ǃ"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar Click) = "ǁ̬"
phoneToString (P Voiceless (Tongue Apical Lateral) Alveolar Click) = "ǁ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Stop Oral)) = "d"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Stop Oral)) = "t"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Alveolar (Stop Oral)) = "ɗ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Stop Nasal)) = "n"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Alveolar (Stop Nasal)) = "ɗ̃"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Affricate Sibilant)) = "d͡z"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Affricate Sibilant)) = "t͡s"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Affricate NonSibilant)) = "d͡ð͇"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Affricate NonSibilant)) = "t͡θ͇"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar (Affricate NonSibilant)) = "d͡ɮ"
phoneToString (P Voiceless (Tongue Apical Lateral) Alveolar (Affricate NonSibilant)) = "t͡ɬ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Fricative Sibilant)) = "z"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Fricative Sibilant)) = "s"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar (Fricative NonSibilant)) = "ð͇"
phoneToString (P Voiceless (Tongue Apical NonLateral) Alveolar (Fricative NonSibilant)) = "θ͇"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar (Fricative NonSibilant)) = "ɮ"
phoneToString (P Voiceless (Tongue Apical Lateral) Alveolar (Fricative NonSibilant)) = "ɬ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Trill) = "r"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Tap) = "ɾ"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar Tap) = "ɺ"
phoneToString (P Voiced (Tongue Apical NonLateral) Alveolar Approximant) = "ɹ"
phoneToString (P Voiced (Tongue Apical Lateral) Alveolar Approximant) = "l"
-- Postalveolars
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Click) = "ǃ̬"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar Click) = "ǃ"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Stop Oral)) = "d̠"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Stop Oral)) = "t̠"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Postalveolar (Stop Oral)) = "ɗ̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Stop Nasal)) = "n̠"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Postalveolar (Stop Nasal)) = "ɗ̠̃"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Affricate Sibilant)) = "d͡ʒ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Affricate Sibilant)) = "t͡ʃ"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Affricate Sibilant)) = "d͡ɹ̠˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Affricate Sibilant)) = "t͡ɹ̠̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Fricative Sibilant)) = "ʒ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Fricative Sibilant)) = "ʃ"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar (Fricative NonSibilant)) = "ɹ̠˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Postalveolar (Fricative NonSibilant)) = "ɹ̠̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Trill) = "r̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Tap) = "ɾ̠"
phoneToString (P Voiced (Tongue Apical NonLateral) Postalveolar Approximant) = "ɹ̠"
phoneToString (P Voiced (Tongue Apical Lateral) Postalveolar Approximant) = "l̠"
-- palatoalveolars
phoneToString (P Voiced (Tongue Laminal NonLateral) Postalveolar Click) = "ǂ̬"
phoneToString (P Voiceless (Tongue Laminal NonLateral) Postalveolar Click) = "ǂ"
phoneToString (P Voiced (Tongue Laminal NonLateral) Postalveolar (Affricate Sibilant)) = "d͡ʑ"
phoneToString (P Voiceless (Tongue Laminal NonLateral) Postalveolar (Affricate Sibilant)) = "t͡ɕ"
phoneToString (P Voiced (Tongue Laminal NonLateral) Postalveolar (Fricative Sibilant)) = "ʑ"
phoneToString (P Voiceless (Tongue Laminal NonLateral) Postalveolar (Fricative Sibilant)) = "ɕ"
-- Retroflexes
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Stop Oral)) = "ɖ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Stop Oral)) = "ʈ"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Palatal (Stop Oral)) = error "No voiced ingressive retroflex stop"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Stop Nasal)) = "ɳ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Affricate Sibilant)) = "ɖ͡ʐ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Affricate Sibilant)) = "ʈ͡ʂ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Affricate NonSibilant)) = "ɖ͡ɻ˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Affricate NonSibilant)) = "ʈ͡ɻ̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Fricative Sibilant)) = "ʐ"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Fricative Sibilant)) = "ʂ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal (Fricative NonSibilant)) = "ɻ˔"
phoneToString (P Voiceless (Tongue Apical NonLateral) Palatal (Fricative NonSibilant)) = "ɻ̊˔"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal Trill) = error "No retroflex trill"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal Tap) = "ɽ"
phoneToString (P Voiced (Tongue Apical NonLateral) Palatal Approximant) = "ɻ"
phoneToString (P Voiced (Tongue Apical Lateral) Palatal Approximant) = "ɭ"
-- Palatals
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Oral)) = "ɟ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Oral)) = "c"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Oral)) = "ʄ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Nasal)) = "ɲ"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Palatal (Stop Nasal)) = "ʄ̃"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Affricate NonSibilant)) = "ɟ͡ʝ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Palatal (Affricate NonSibilant)) = "c͡ç"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Fricative NonSibilant)) = "ʝ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Palatal (Fricative NonSibilant)) = "ç"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal Approximant) = "j"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Palatal Approximant) = "ɥ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) Lateral) Palatal Approximant) = "ʎ"
-- Velars
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Oral)) = "ɡ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Oral)) = "k"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Oral)) = "ɠ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Nasal)) = "ŋ"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Velar (Stop Nasal)) = "ɠ̃"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Affricate NonSibilant)) = "ɡ͡ɣ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Velar (Affricate NonSibilant)) = "k͡x"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar (Fricative NonSibilant)) = "ɣ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Velar (Fricative NonSibilant)) = "x"
phoneToString (P Voiceless (Tongue (Dorsal Rounded) NonLateral) Velar (Fricative NonSibilant)) = "ʍ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar Approximant) = "ɰ"
phoneToString (P Voiced (Tongue (Dorsal Rounded) NonLateral) Velar Approximant) = "w"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) Lateral) Velar Approximant) = "ʟ"
-- Uvulars
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Oral)) = "ɢ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Oral)) = "q"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Oral)) = "ʛ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Nasal)) = "ɴ"
phoneToString (P VoicedIngressive (Tongue (Dorsal Unrounded) NonLateral) Uvular (Stop Nasal)) = "ʛ̃"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Affricate NonSibilant)) = "ɢ͡ʁ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Uvular (Affricate NonSibilant)) = "q͡χ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular (Fricative NonSibilant)) = "ʁ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Uvular (Fricative NonSibilant)) = "χ"
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Uvular Trill) = "ʀ"
-- Pharyngeals
phoneToString (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Pharyngeal (Fricative NonSibilant)) = "ʕ"
phoneToString (P Voiceless (Tongue (Dorsal Unrounded) NonLateral) Pharyngeal (Fricative NonSibilant)) = "ħ"
-- Epiglottals
phoneToString (P Voiceless Epiglottal Pharyngeal (Stop Oral)) = "ʡ"
phoneToString (P Voiced Epiglottal Pharyngeal (Fricative NonSibilant)) = "ʢ"
phoneToString (P Voiceless Epiglottal Pharyngeal (Fricative NonSibilant)) = "ʜ"
-- Glottals
phoneToString (P Voiceless NoActive NoPassive (Stop Oral)) = "ʔ"
phoneToString (P Voiceless NoActive NoPassive (Fricative NonSibilant)) = "h"
-- ono :(
phoneToString (P {}) = error "AAAAH!!! I missed a case!"

a :: [(Int,Bool)]
a = [ (p,x == [y]) | (p,(x,y)) <- zip [0..] (zip (map (phoneToString . charToPhone) universe) universe)]
