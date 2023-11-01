module Rules(
    modusPonens
) where

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


modusPonens :: Prop -> Prop -> Prop
modusPonens (p :->: q) z 
    | p == z = q
    | otherwise = F
modusPonens z (p :->: q) = modusPonens (p :->: q) z 
modusPonens _ _ = F 

modusTollens ::Prop -> Prop -> Prop
modusTollens (p :->: q) (Not z) 
    | q == z = Not p
    | otherwise = F
modusTollens (Not z) (p :->: q) = modusTollens (p :->: q) (Not z) 
modusTollens _ _ = F

silogismeHipotetik ::Prop -> Prop -> Prop
silogismeHipotetik (p :->: q) (z :->: r) 
    | q == z = (p :->: r)
    | r == p = (z :->: q)
    | otherwise = F
silogismeHipotetik _ _ = F

silogismeDisjungtif ::Prop -> Prop -> Prop 
silogismeDisjungtif (p :|: q) (Not z) 
    | p == z = q
    | otherwise = F
silogismeDisjungtif (Not z) (p :|: q) = silogismeDisjungtif (p :|: q) (Not z) 
silogismeDisjungtif _  _ = F
