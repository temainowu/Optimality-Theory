data PhoneData = P GlottalState Active Passive Manner | SyllableBoundary | MorphemeBoundary | WordBoundary
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
unvoiced = "pɸʍfθtsɬʃʈʂcçkxqχħhʜʢʡ"
voiced = "bmʙβwɱⱱvʋðdnrɾɺzɮɹlʒɕʑɖɳɽʐɻɭɟɲʝjɥʎgŋɣɰʟɢɴʀʁʕieɛæɶɪyøœɨɵəɐaɯɤʌɑʊuoɔɒ"
creaky = ""
breathy = "ɦ"
closed = "ʔ"
ingressive = "ɓɗʄɠʛ"

-- active articulators
inflab = "pbmʙɸβɱⱱfvʋʘɓ"
api = "θðtdnrɾɺszɬɮɹlʃʒʈɖɳɽʂʐɻɭǀǃǁɗ"
lam = "ɕʑǂ"
dors = "ʍcɟɲçʝjɥʎkgŋxɣɰwʟqɢɴʀχʁħʕʄɠʛieɛæɪyøœɶɨɵəɐaɯɤʌɑʊuoɔɒ"
epi = "ʜʢʡ"

-- passive articulators
suplab = "pbmʙɸβʘɓ"
dent = "ɱⱱfvʋθðǀ"
alv = "tdnrɾɺszɬɮɹlǁɗ"
postalv = "ʃʒɕʑǃǂ"
pal = "ʈɖɳɽʂʐɻɭcɟɲçʝjɥʎʄieɛæɶɪyøœ"
cent = "ɨɵəɐa"
vel = "ʍkgŋxɣɰwʟɠɯɤʌɑʊuoɔɒ"
uvul = "qɢɴʀχʁʛ"
phar = "ħʕʜʢʡ"

-- manners
click = "ʘǀǃǂǁ"
stop = "pbtdʈɖcɟkgqɢʔʡ"
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

charToPhone :: Char -> PhoneData
charToPhone '.' = SyllableBoundary
charToPhone '+' = MorphemeBoundary
charToPhone '#' = WordBoundary
charToPhone x = P (glottalStateOf x) (activeOf x) (passiveOf x) (mannerOf x)


phoneToString :: PhoneData -> String
phoneToString WordBoundary = "#"
phoneToString MorphemeBoundary = "+"
phoneToString SyllableBoundary = "."
-- Errors
phoneToString (P _                  (Tongue (Dorsal _) NonLateral) Palatal (Vowel Low _))        = error "No palatal low vowels"
phoneToString (P _                  (Tongue (Dorsal _) NonLateral) Velar   (Vowel Low _))        = error "No velar low vowels"
phoneToString (P _                  (Tongue (Dorsal _) Lateral)    _       (Vowel Low _))        = error "No lateral vowels"
phoneToString (P Closed             _                              _       (Vowel _   _))        = error "No ejective vowels"
phoneToString (P VoicelessAspirated _                              _       (Vowel _   _))        = error "No aspirated vowels"
phoneToString (P VoicedIngressive   _                              _       Click)                = error "No voiced ingressive click"
phoneToString (P VoicedIngressive   _                              _       (Stop Nasal))         = error "No voiced ingressive nasal"
phoneToString (P VoicedIngressive   _                              _       (Affricate _))        = error "No voiced ingressive affricates"
phoneToString (P VoicedIngressive   _                              _       (Fricative NonSibilant)) = error "No voiced ingressive fricative"
phoneToString (P VoicedIngressive   _                              _       Trill)                   = error "No voiced ingressive trill"
phoneToString (P VoicedIngressive   _                              _       Tap)                     = error "No voiced ingressive tap"
phoneToString (P VoicedIngressive   _                              _       Approximant)             = error "No voiced ingressive approximant"
phoneToString (P Closed             _                              _       (Stop Nasal))            = error "No ejective nasal"
phoneToString (P _                  Inferiolabial                  _       (Affricate Sibilant))    = error "No Labial Sibilant Affricate"
phoneToString (P _                  Inferiolabial                  _       (Fricative Sibilant))    = error "No Labial Sibilant Fricative"
phoneToString (P _                  Inferiolabial                  Superiolabial Tap)               = error "No Bilabial Tap"
phoneToString (P _                  Inferiolabial                  Superiolabial Approximant)       = error "No Bilabial Approximant"
phoneToString (P _                  Inferiolabial                  Dental        Click)             = error "No labiodental Click"
phoneToString (P _                  Inferiolabial                  Dental        (Stop Oral))       = error "No labiodental Stop"

-- recursive cases
phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height Nasal)) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height Oral)) ++ "̃"
phoneToString (P Voiceless (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) ++ "̥"
phoneToString (P Creaky a p m) = phoneToString (P Voiced a p m) ++ "̰"
phoneToString (P VoicedIngressive (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) = phoneToString (P Voiced (Tongue (Dorsal rounding) NonLateral) place (Vowel height nasality)) ++ "↓"
phoneToString (P VoicelessAspirated a p m) = phoneToString (P Voiceless a p m) ++ "ʰ"
phoneToString (P Closed a p (Stop Oral)) = phoneToString (P Voiceless a p (Stop Oral)) ++ "ʼ"
phoneToString (P Closed a p (Affricate s)) = phoneToString (P Voiceless a p (Affricate s)) ++ "ʼ"
phoneToString (P Closed a p (Fricative s)) = phoneToString (P Voiceless a p (Fricative s)) ++ "ʼ"
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
phoneToString (P Closed Inferiolabial Superiolabial Click) = "ʘ"
phoneToString (P Voiced Inferiolabial Superiolabial (Stop Oral)) = "b"
phoneToString (P Voiceless Inferiolabial Superiolabial (Stop Oral)) = "p"
phoneToString (P VoicedIngressive Inferiolabial Superiolabial (Stop Oral)) = "ɓ"
phoneToString (P Voiced Inferiolabial Superiolabial (Stop Nasal)) = "m"
phoneToString (P Voiceless Inferiolabial Superiolabial (Stop Nasal)) = "m̥"
phoneToString (P VoicedIngressive Inferiolabial Superiolabial (Stop Nasal)) = "ɓ̃" -- not sure about this one
phoneToString (P Voiced Inferiolabial Superiolabial (Affricate NonSibilant)) = "b͡β"
phoneToString (P Voiceless Inferiolabial Superiolabial (Affricate NonSibilant)) = "p͡ɸ"
phoneToString (P Voiced Inferiolabial Superiolabial (Fricative NonSibilant)) = "β"
phoneToString (P Voiceless Inferiolabial Superiolabial (Fricative NonSibilant)) = "ɸ"
phoneToString (P Voiced Inferiolabial Superiolabial Trill) = "ʙ"
phoneToString (P Voiceless Inferiolabial Superiolabial Trill) = "ʙ̥"
phoneToString (P Closed Inferiolabial Superiolabial Trill) = "ʙ̥ʼ" -- not sure about this one
-- Labiodentals
phoneToString (P voiced Inferiolabial Dental (Stop Nasal)) = "ɱ"
phoneToString (P Voiceless Inferiolabial Dental (Stop Nasal)) = "ɱ̥"
phoneToString (P Voiced Inferiolabial Dental (Affricate NonSibilant)) = "b͡v"
phoneToString (P Voiceless Inferiolabial Dental (Affricate NonSibilant)) = "p͡f"
phoneToString (P Voiced Inferiolabial Dental (Fricative NonSibilant)) = "v"
phoneToString (P Voiceless Inferiolabial Dental (Fricative NonSibilant)) = "f"
phoneToString (P _ Inferiolabial Dental Trill) = error "No labiodental trill"
phoneToString (P Voiced Inferiolabial Dental Tap) = "ⱱ"
phoneToString (P Voiceless Inferiolabial Dental Tap) = "ⱱ̥"
phoneToString (P Closed _ _ Tap) = error "No ejective tap"
phoneToString (P Voiced Inferiolabial Dental Approximant) = "ʋ"
phoneToString (P Voiceless Inferiolabial Dental Approximant) = "ʋ̥"
phoneToString (P Closed _ _ Approximant) = error "No ejective approximant"
-- Dentals
phoneToString (P Voiced (Tongue Apical NonLateral) Dental Click) = "ǀ̬" -- not sure about this one
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental Click) = "ǀ"
phoneToString (P Closed (Tongue Apical NonLateral) Dental Click) = "ǀ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Stop Oral)) = "d̪"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Stop Oral)) = "t̪"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Dental (Stop Oral)) = "ɗ̪"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Stop Nasal)) = "n̪"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Stop Nasal)) = "n̪̊"
phoneToString (P VoicedIngressive (Tongue Apical NonLateral) Dental (Stop Nasal)) = "ɗ̪̃" -- not sure about this one
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Affricate NonSibilant)) = "d̪͡ð"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Affricate NonSibilant)) = "t̪͡θ"
phoneToString (P Voiced (Tongue Apical NonLateral) Dental (Fricative NonSibilant)) = "ð"
phoneToString (P Voiceless (Tongue Apical NonLateral) Dental (Fricative NonSibilant)) = "θ"
phoneToString (P {}) = error "AAAAH!!! I missed a case!"

a :: [String]
a = map (phoneToString . charToPhone) universe
