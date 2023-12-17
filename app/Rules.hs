{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Rules where

type Name = String
data Predicate = Is Name 
                | ElementOf Name
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
simplifikasi _ = F

resolusi :: Prop -> Prop -> Prop
resolusi (p :|: q) (z :|: r)
-- TODO: swapcase
    | z == (~) p = q :|: r
    | otherwise = F
resolusi _ _ = F

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
modusPonensUniversal (ForAll domain prop@(p :->: (_:~:pred)))(name :~: predicate)(name2 :~: (ElementOf d))
    | domain == d && name == name2 = name :~: pred
    | otherwise = F
modusPonensUniversal (ForAll domain prop) (name2 :~: (ElementOf d))(name :~: predicate) = modusPonensUniversal (ForAll domain prop)(name :~: predicate)(name2 :~: (ElementOf d))
modusPonensUniversal (name2 :~: (ElementOf d)) (ForAll domain prop) (name :~: predicate) = modusPonensUniversal (ForAll domain prop)(name :~: predicate)(name2 :~: (ElementOf d))
modusPonensUniversal (name2 :~: (ElementOf d)) (name :~: predicate) (ForAll domain prop) = modusPonensUniversal (ForAll domain prop)(name :~: predicate)(name2 :~: (ElementOf d))
modusPonensUniversal (name :~: predicate) (name2 :~: (ElementOf d)) (ForAll domain prop) = modusPonensUniversal (ForAll domain prop)(name :~: predicate)(name2 :~: (ElementOf d))
modusPonensUniversal (name :~: predicate) (ForAll domain prop) (name2 :~: (ElementOf d)) = modusPonensUniversal (ForAll domain prop)(name :~: predicate)(name2 :~: (ElementOf d))
modusPonensUniversal _ _ _ = F