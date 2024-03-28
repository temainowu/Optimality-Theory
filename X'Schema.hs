module OptimalityTheory.X'Schema where

import OptimalityTheory.Phones

class Phrase p where
    symbol :: p -> Char
    toPhrase :: (h,c,a,s) -> Phrases h c a s

class Specifier 

data XP = XP X' [XP]
data X' = X' X' XP | X0 X [XP]
data X = C [Foot] 
       | T [Foot]  
       | F [Foot]  
       | M [Foot]  
       | I [Foot]  
       | B [Foot]  
       | V [Foot]  
       | Q [Foot]  
       | D [Foot]  
       | G [Foot]  
       | L [Foot]  
       | S [Phone]
data Foot = Foot [S] [Feature]
data Feature = Harmony [Height] | Tone [Height]

instance Eq X where
    (C _) == (C _) = True
    (T _) == (T _) = True
    (F _) == (F _) = True
    (M _) == (M _) = True
    (I _) == (I _) = True
    (B _) == (B _) = True
    (V _) == (V _) = True
    (Q _) == (Q _) = True
    (D _) == (D _) = True
    (G _) == (G _) = True
    (L _) == (L _) = True
    (S _) == (S _) = True
    _ == _ = False

same :: [a] -> Bool
same [] = True
same (x:xs) = all (== x) xs

getComplement :: X' -> Maybe XP
getComplement (X' x _) = getComplement x
getComplement (X0 x []) = Nothing
getComplement (X0 x (y:ys)) | same (y:ys) = Just (y)
                            | otherwise = XP ((X0 (C []) []) []) []

complement :: X -> X
complement (C _) = T []
complement (T _) = F []
complement (F _) = M []
complement (M _) = I []
complement (I _) = B []
complement (B _) = V []
complement (Q _) = D []
complement (D _) = G []
complement (S _) = S []
complement _ = L []

invariant :: XP -> Bool
invariant (XP x xs) = null (getComplement x) || getComplement x == L [] || (getComplement x) == complement (getComplement x)
    where comp = case getComplement x of
                     Nothing -> True
                     Just x' -> x' == complement x'

strengths = XP (X' ) []
 
{-

data Word = Phoneme Phone

type Phrase = (String, [Phrase], [Phrase], Maybe Phrase)


data Direction = L | R

data Phrases = Null | Syllable (Phone, [(Phrases,Direction)], [(Phrases,Direction)], [(Phrases,Direction)])

instance Show (Phrases h) where
    show [] = []
    show ((p,_):ps) = phraseShow p
        where
            phraseShow :: (Phrase h) => (h, Phrases c, Phrases a, Phrases s) -> String
            phraseShow Nothing = ""
            phraseShow (Just (h,c,a,[]))       = '[':(symbol h):'P':' ': barShow h c a                                ++ "]"
            phraseShow (Just (h,c,a,(s,L):ss)) = '[':(symbol h):'P':' ': phraseShow s ++ phraseShow (Just (h,ss,c,a)) ++ "]"
            phraseShow (Just (h,c,a,(s,R):ss)) = '[':(symbol h):'P':' ': phraseShow (Just (h,ss,c,a)) ++ phraseShow s ++ "]"

            barShow :: (Phrase h) => h -> Phrases c -> Phrases a -> String
            barShow h c []           = '[':(symbol h):'\'':' ': headShow h c               ++ "]"
            barShow h c (a@(_,L):as) = '[':(symbol h):'\'':' ': show [a] ++ barShow h c as ++ "]"
            barShow h c (a@(_,R):as) = '[':(symbol h):'\'':' ': barShow h c as ++ show [a] ++ "]"

            headShow :: (Phrase h) => h -> Phrases c -> String
            headShow h []           = '[':(symbol h):' ': show h                    ++ "]"
            headShow h (c@(_,L):cs) = '[':(symbol h):' ': show [c] ++ headShow h cs ++ "]"
            headShow h (c@(_,R):cs) = '[':(symbol h):' ': headShow h cs ++ show [c] ++ "]"

type Syllable = Phrases Phone

instance Phrase Syllable where
    symbol _ = 'S'
    toPhrase (h,c::P,a,s) = [(h, map (doToFst toPhrase) c, map (doToFst toPhrase) a, map (doToFst toPhrase) s),L] 

doToFst :: (a -> b) -> (a,c) -> (b,c)
doToFst f (a,b) = (f a,b)

toPhrase :: (h,c,a,s) -> Phrases h c a s
toPhrase (h,c,a,s) = [(h, map (doToFst toPhrase) c, map (doToFst toPhrase) a, map (doToFst toPhrase) s),L]

strengths :: Syllable
strengths = toPhrase (P Voiced (Tongue (Dorsal Unrounded) NonLateral) Palatal (Vowel Mid) Oral,[((P Voiced (Tongue (Dorsal Unrounded) NonLateral) Velar NasalStop Nasal,[],[],[]),R)],[((P Voiceless (Tongue Apical NonLateral) Dental (Fricative NonSibilant) Oral,[],[],[]),R),((P Voiceless (Tongue Apical NonLateral) Alveolar (Fricative Sibilant) Oral,[],[],[]),R)],[((P Voiceless (Tongue Apical NonLateral) Alveolar (Stop Tenuis) Oral,[(P Voiceless (Tongue Apical NonLateral) Alveolar (Fricative Sibilant) Oral,[],[],[]),R],[(([],P Voiced (Tongue Apical NonLateral) Alveolar Approximant Oral,[],[]),R)],[]),L)])
-}
