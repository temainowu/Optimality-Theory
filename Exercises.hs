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
-- Exercise 2.3 - Voicing assimilation in Dutch (90)
-- Exercise 3.1 - -*um*-infixation in Ilokano (141)
-- Exercise 3.1 - Epenthesis in Harari (141)
-- Exercise 4.1 - Wargamay (191)
-- Exercise 4.2 - Manam (191)
-- Exercise 4.3 - Murinbata (191)
-- Exercise 4.4 - Warlpiri (192)
-- Exercise 4.5 - A factorial typology of stress systems (193)
-- Exercise 5.1 - Reduplication in Axininca Campa (254)
-- Exercise 5.2 - Reduplication in Oykangand (255)
-- Exercise 5.3 - Infixing reduplication in Pangasinan (255)
-- Exercise 5.4 - Infixing reduplication in Mangarayi (256)
-- Exercise 5.5 - A factorial typology of reduplicative systems (256)
-- Exercise 6.1 - Spanish (294)
-- Exercise 6.2 - Belfast English (295)
-- Exercise 6.3 - Factorial typology of OO-correspondence (295)
-- Exercise 7.1 - Why the learner must use stratified hierarchies (339)
-- Exercise 7.2 - Learning input forms under positional faithfulness (340)