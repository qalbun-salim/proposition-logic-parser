{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Rules where

type Name = String
data Predicate = Is Name 
                | ElementOf Name
                | NotPredicate Predicate
                deriving (Eq, Ord, Show)

data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          | ForAll Name Prop
          | Exist Name Predicate
          | Prop :~: Predicate
          deriving (Eq, Ord, Show)

-- Operator
-- infixl 9 `:&:`
-- infixl 8 `:||:`
-- infixl 7 `:->:`
-- infixl 6 `:<->:`

(~) :: Prop -> Prop
(~) (Not p) = p
(~) p = Not p

-- Propotional Logic Rules

modusPonens :: Prop -> Prop -> Prop
modusPonens (p :->: q) z
    | p == z = q
    | otherwise = F
modusPonens z (p :->: q) = modusPonens (p :->: q) z
modusPonens _ _ = F

modusTollens ::Prop -> Prop -> Prop
modusTollens (p :->: q) z
    | q == (~) z = (~) p
    | otherwise = F
modusTollens z (p :->: q) = modusTollens (p :->: q) z
modusTollens _ _ = F

silogismeHipotetik ::Prop -> Prop -> Prop
silogismeHipotetik (p :->: q) (z :->: r)
    | q == z = p :->: r
    | r == p = z :->: q
    | otherwise = F
silogismeHipotetik _ _ = F

silogismeDisjungtif ::Prop -> Prop -> Prop
silogismeDisjungtif (p :|: q) z
    | p == (~) z = q
    | otherwise = F
silogismeDisjungtif z (p :|: q) = silogismeDisjungtif (p :|: q) z
silogismeDisjungtif _  _ = F

simplifikasi :: Prop ->  Prop
-- TODO: swap case , F case
simplifikasi (p :&: q) = p

resolusi :: Prop -> Prop -> Prop
resolusi (p :|: q) (z :|: r)
-- TODO: swapcase
    | z == (~) p = q :|: r
    | otherwise = F
resolusi _ _ = F

-- Predicate Logic Rules

-- showPredicate :: Prop -> Predicate
-- showPredicate (ForAll name predicate) = predicate

instansiasiUniversal :: Prop -> Prop -> Prop
instansiasiUniversal (ForAll domain p@(_ :~: predicate)) (name :~: pred@(ElementOf d)) 
    | domain == d = name :~: predicate
    | otherwise = F
instansiasiUniversal (name :~: pred) (ForAll domain predicate) = instansiasiUniversal (ForAll domain predicate) (name :~: pred)  
instansiasiUniversal _ _ = F

-- instansiasiEksistensial :: Prop -> Prop
-- instansiasiEksistensial (Exist domain predicate) = Var "C" :~: predicate
-- instansiasiEksistensial _ _ = F

generalisasiEksistensial :: Prop -> Prop -> Prop
generalisasiEksistensial (name :~: predicate)(name2 :~: pred@(ElementOf d)) 
    | name == name2 = (Exist d predicate)
    | otherwise = F
generalisasiEksistensial (name2 :~: (ElementOf d)) (name :~: predicate) = generalisasiEksistensial (name :~: predicate)(name2 :~: (ElementOf d)) 
generalisasiEksistensial _ _ = F

modusPonensUniversal :: Prop -> Prop -> Prop -> Prop
modusPonensUniversal (prop1)(prop2)(prop3) = 
    let porplist = filter (/= F) (fmap extractPredicate [prop1, prop2, prop3]) 
        finalResult = if applyRules modusPonens porplist == F then F else T
    in finalResult


    
extractPredicate :: Prop -> Prop
extractPredicate = ep where
    ep (_ :~: Is x) = Var x
    ep (_ :~: NotPredicate (Is x)) = Not (Var x)
    ep (p1 :->: p2) = (ep (p1) :->: ep (p2))
    ep (ForAll _ p) = ep p 
    ep _ = F

applyRules :: (Prop -> Prop -> Prop) -> [Prop] -> Prop
applyRules _ [] = F
applyRules f [x] = x
applyRules f (x:xs) = f x (applyRules f xs)