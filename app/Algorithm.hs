{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Algorithm where

import Rules
import qualified Data.Set as Set

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x:xs) = [(x, y) | y <- xs] ++ pairwise xs

-- computeSolution :: (Prop,Prop) -> [Prop]
-- computeSolution x = filter (/= F) lst where
--     lst = map ($ x) (fmap uncurry [modusTollens,modusPonens,silogismeHipotetik,silogismeDisjungtif,resolusi])

computeSolution :: [Prop] -> [Prop]
computeSolution x = filter (/= F) $ solverThree x ++ solverTwo x ++ solverOne x 

solveOneProp :: [Prop->Prop] -> [Prop] -> [Prop]
solveOneProp props prems = props <*> prems

solverOne =  solveOneProp [simplifikasi]

solveTwoProps :: [Prop->Prop->Prop] -> [Prop] -> [Prop]
solveTwoProps props prems = props <*> prems <*> prems

solverTwo = solveTwoProps [modusTollens,modusPonens,silogismeHipotetik,silogismeDisjungtif,resolusi,instansiasiUniversal,generalisasiEksistensial]

solveThreeProps :: [Prop->Prop->Prop->Prop] -> [Prop] -> [Prop]
solveThreeProps props prems = props <*> prems <*> prems <*> prems

solverThree = solveThreeProps [modusPonensUniversal]

-- flattenSolution :: [Prop] -> [Prop]
-- flattenSolution x = concatMap computeSolution (pairwise x)

calculateSolution :: [Prop] -> Prop -> Bool
calculateSolution premises hyp | hyp `elem` (premises ++ computeSolution premises) = True
                       | Set.fromList premises == Set.fromList (premises ++ computeSolution premises)  = False
                       | otherwise = calculateSolution (premises ++ computeSolution premises) hyp
