module Exercises where

import OT 
import Phones

newtype PrettyList a = PrettyList [a]

instance Show a => Show (PrettyList a) where
    show (PrettyList []) = ""
    show (PrettyList (x:xs)) = show x ++ '\n' : show (PrettyList xs)

-- Exercises from Kager (1999)

-- Exercise 1.1
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

-- Exercise 1.2
exe1'2 :: Tableau
exe1'2 = makeTableau
  [ident place,        agree obsVoice,    ident obsVoice,   noVoicedObstruents]
  ["Ident-IO(Place)", "Agree(ObsVoice)", "Ident-IO(voice)", "*ObsVoice"]
  "katz" ["katz", "kats", "kadz", "kads", "dogz"]

-- Exercise 2.1

-- /pemp-o/ → pembo "I send"
-- /ton#topo/ → tondopo "the place"
-- /e-pemp-sa/ → epepsa "I sent (aorist)"
-- /ton#psefti/ → topsefti "the liar (Cypriot dialect)"

constraints2'1 = []

labels2'1 = []

exe2'1 :: PrettyList Tableau
exe2'1 = PrettyList [
    makeTableau constraints2'1 labels2'1 "pempo"   ["pempo", "pembo", "pepo"],
    makeTableau constraints2'1 labels2'1 "epempsa"   ["epempsa", "epepsa", "epembsa", "epembza"]]