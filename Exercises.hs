module Exercises where

import OT
import Phones

newtype PrettyList a = PrettyList [a]

instance Show a => Show (PrettyList a) where
    show (PrettyList []) = ""
    show (PrettyList (x:xs)) = show x ++ '\n' : show (PrettyList xs)

-- Exercises from Kager (1999)

-- Exercise 1.1 - Japanese (50)
nowbŋ :: Constraint
nowbŋ _ ((P _ (Tongue _ Dorsal _) _ NasalStop Nasal, _):o) = 1
nowbŋ _ _ = 0

constraints1'1 = [nowbŋ, noVoicedObstruents, ident manner]
labels1'1 = ["*#ŋ", "*ObsVoice", "Ident-IO(Manner)"]

exe1'1 :: PrettyList Tableau
exe1'1 = PrettyList [
    makeTableau constraints1'1 labels1'1 "geta"   ["geta",   "ŋeta"],
    makeTableau constraints1'1 labels1'1 "giri"   ["giri",   "ŋiri"],
    makeTableau constraints1'1 labels1'1 "gutʃi"  ["gutʃi",  "ŋutʃi"],
    makeTableau constraints1'1 labels1'1 "go"     ["go",     "ŋo"],
    makeTableau constraints1'1 labels1'1 "kagi"   ["kagi",   "kaŋi"],
    makeTableau constraints1'1 labels1'1 "kago"   ["kago",   "kaŋo"],
    makeTableau (agree nasalObs:ident place:constraints1'1) ("Agree(NC-Place)":"Ident-IO(Place)":labels1'1) "kangae" ["kangae", "kaŋŋae", "kaŋgae"],
    makeTableau constraints1'1 labels1'1 "tokage" ["tokage", "tokaŋe"]]

-- Exercise 1.2 - English (51)
exe1'2 :: Tableau
exe1'2 = makeTableau
  [ident place,        agree obsVoice,    ident obsVoice,   noVoicedObstruents]
  ["Ident-IO(Place)", "Agree(ObsVoice)", "Ident-IO(voice)", "*ObsVoice"]
  "katz" ["katz", "kats", "kadz", "kads", "dogz"]

-- Exercise 2.1 - A *NC̥ conspiracy in Modern Greek (88)

-- /pemp-o/ → pembo "I send"
-- /ton#topo/ → tondopo "the place"
-- /e-pemp-sa/ → epepsa "I sent (aorist)"
-- /ton#psefti/ → topsefti "the liar (Cypriot dialect)"

constraints2'1 = [agree nasalObsVoice, agree obsVoice, ident fricVoice, maxi]

labels2'1 = ["*NC", "Agree(ObsVce)", "Ident-IO(FricVce)", "Max-IO"]

exe2'1 :: PrettyList Tableau
exe2'1 = PrettyList [
    makeTableau constraints2'1 labels2'1 "pempo"   ["pempo", "pembo", "pepo"],
    makeTableau constraints2'1 labels2'1 "epempsa" ["epempsa", "epepsa", "epembsa", "epembza"]]

-- something like /pemp-ta/ would become [pembda] not [pepta] under these constraints
-- I don't know if this is testible in Greek

-- Exercise 2.2 - Kikuyu Verbs (89)

constraints2'2 = [agree nasalObs, agree continuancy, ident manner, agree nasalObsVoice, noVoicedStops]

labels2'2 = ["agree(NC-place)", "Agree(Continuancy)", "Ident-IO(Continuancy)", "*NC_o", "*VoicedStop"]

exe2'2 :: PrettyList Tableau
exe2'2 = PrettyList [
    makeTableau constraints2'2 labels2'2 "bura"     ["βura", "bura"],
    makeTableau constraints2'2 labels2'2 "ɴbureetɛ" ["mbureetɛ", "ɴbureetɛ"],
    makeTableau constraints2'2 labels2'2 "koma"     ["koma", "goma"],
    makeTableau constraints2'2 labels2'2 "ɴkomɛɛtɛ" ["ŋgomɛɛtɛ", "ɴkomɛɛtɛ", "ɴgomɛɛtɛ", "ŋkomɛɛtɛ"],
    makeTableau constraints2'2 labels2'2 "ɣora"     ["ɣora", "gora"],
    makeTableau constraints2'2 labels2'2 "ɴɣoreetɛ" ["ŋgoreetɛ", "ɴɣoreetɛ", "ɴgoreetɛ", "ŋɣoreetɛ"]]

-- Exercise 2.3 - Voicing assimilation in Dutch (90)

constraints2'3 = [agree obsVoice, ident stopVoice, noVoicedObstruents, ident obsVoice, noVoicedStops]
labels2'3 = ["Agree(ObsVce)", "Ident-IO(StopVoice)", "*Voiced Obs", "Ident-IO(ObsVce)", "*Voiced Stop"]

-- optimal form is first in each list of candidates
exe2'3 :: PrettyList Tableau
exe2'3 = PrettyList [
        makeTableau constraints2'3 labels2'3 "pd" ["bd","pt","pd"], -- ss uv (stropdas)
        makeTableau constraints2'3 labels2'3 "dk" ["tk","dg","dk"], -- ss vu (bloedkoraal)
        makeTableau constraints2'3 labels2'3 "sb" ["zb","sp","sb"], -- fs uv (kasboek)
        makeTableau constraints2'3 labels2'3 "zp" ["sp","zb","zp"], -- fs vu (kaaspers)
        makeTableau constraints2'3 labels2'3 "kv" ["kf","gv","kv"], -- sf uv (boekvorm)
        makeTableau constraints2'3 labels2'3 "sv" ["sf","zv","sv"], -- ff uv (bosveen)
        makeTableau constraints2'3 labels2'3 "dv" ["tf","dv","tv","df"], -- ss vv (handvat)
        makeTableau constraints2'3 labels2'3 "vz" ["fs","vz","fz","vs"] -- ff vv (drijfzand)
    ]

-- Exercise 3.1 - -*um*-infixation in Ilokano (141)

-- free variation between "gumradwet" and "grumadwet" from input /um-gradwet/
exe3'1 :: Tableau
exe3'1 = Tableau
    [noCoda, \ _ -> fromEnum . (0 `notElem`) . snd . head, linearity]
    ["No-Coda", "Align-um-L", "Linearity-IO"]
    "umgradwet"
    [toLexeme "umgradwet", (charToPhone 'g', [2]) : (charToPhone 'u', [0]) : (charToPhone 'm', [1]) : toLexeme "radwet",
    (charToPhone 'g', [2]) : (charToPhone 'r', [3]) : (charToPhone 'u', [0]) : (charToPhone 'm', [1]) : toLexeme "adwet"]

-- Exercise 3.1 - Epenthesis in Harari (141)

ctrs3'2 = [noCoda, dep]
lbls3'2 = ["No-Coda", "Dep-IO"]

exe3'2 :: PrettyList Tableau 
exe3'2 = PrettyList [
    Tableau ctrs3'2 lbls3'2 "tsabr" [
        parToLexeme [('t',[0]),('i',[]),('.',[]),('s',[1]),('a',[2]),('.',[]),('b',[3]),('r',[4]),('i',[])],
        parToLexeme [('t',[0]),('s',[1]),('a',[2]),('b',[3]),('r',[4])], 
        parToLexeme [('t',[0]),('i',[]),('.',[]),('s',[1]),('a',[2]),('.',[]),('b',[3]),('i',[]),('r',[4])]]
    ]

-- Exercise 4.1 - Wargamay (191)
-- Exercise 4.2 - Manam (191)
-- Exercise 4.3 - Murinbata (191)
-- Exercise 4.4 - Warlpiri (192)
-- Exercise 4.5 - A factorial typology of stress systems (193)

-- Exercise 5.1 - Reduplication in Axininca Campa (254)

-- how to implement Ident-BR???

ctrs5'1 = [linearity, dep]
lbls5'1 = ["Linearity-IO", "Dep-IO"]

exe5'1 :: PrettyList Tableau 
exe5'1 = PrettyList [
    Tableau ctrs5'1 lbls5'1 "osampiR" [
        parToLexeme 
            [('o',[0]),('s',[1]),('a',[2]),('m',[3]),('p',[4]),('i',[5]),
             ('s',[6,1]),('a',[6,2]),('m',[6,3]),('p',[6,4]),('i',[6,5])],
        parToLexeme 
            [('o',[0]),('s',[1]),('a',[2]),('m',[3]),('p',[4]),('i',[5]),
             ('o',[6,0]),('s',[6,1]),('a',[6,2]),('m',[6,3]),('p',[6,4]),('i',[6,5])],
        parToLexeme 
            [('t',[]),('o',[0]),('s',[1]),('a',[2]),('m',[3]),('p',[4]),('i',[5]),
             ('t',[]),('o',[6,0]),('s',[6,1]),('a',[6,2]),('m',[6,3]),('p',[6,4]),('i',[6,5])]
        ]]

-- Exercise 5.2 - Reduplication in Oykangand (255)
-- Exercise 5.3 - Infixing reduplication in Pangasinan (255)
-- Exercise 5.4 - Infixing reduplication in Mangarayi (256)
-- Exercise 5.5 - A factorial typology of reduplicative systems (256)

-- Exercise 6.1 - Spanish (294)

{-
/donθeʎ+a/   → [donθeʎa]
/donθeʎ+a-s/ → [donθeʎas]
/donθeʎ/     → [donθel]
/donθeʎ-s/   → [donθeles]

/desdeɲ+a/   → [desdeɲa]
/desdeɲ+a-s/ → [desdeɲas]
/desdeɲ/     → [desden]
/desdeɲ-s/   → [desdenes]

"+" is a morpheme boundary *within* a stem and 
"-" is a morpheme boundary *outwith* a stem
the relevant markedness constraint is an undominated *stem-final-palatal
-}

-- Exercise 6.2 - Belfast English (295)
-- Exercise 6.3 - Factorial typology of OO-correspondence (295)

-- Exercise 7.1 - Why the learner must use stratified hierarchies (339)
-- Exercise 7.2 - Learning input forms under positional faithfulness (340)