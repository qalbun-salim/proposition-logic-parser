{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Rules where

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord, Show)

-- Operator
-- infixl 9 `:&:`
-- infixl 8 `:||:`
-- infixl 7 `:->:`
-- infixl 6 `:<->:`

(~) :: Prop -> Prop
(~) (Not p) = p
(~) p = Not p

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
simplifikasi (p :&: q) = p

resolusi :: Prop -> Prop -> Prop
resolusi (p :|: q) (z :|: r)
    | z == (~) p = q :|: r
    | otherwise = F
resolusi _ _ = F
