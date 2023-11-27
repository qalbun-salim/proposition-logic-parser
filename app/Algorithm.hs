{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Algorithm where

import Rules

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x:xs) = map (\y -> (x, y)) xs ++ pairwise xs

computeSolution :: (Prop,Prop) -> [Prop]
computeSolution x = filter (/= F) lst where
    lst = map ($ x) (fmap uncurry [modusTollens,modusPonens,silogismeHipotetik,silogismeDisjungtif,resolusi])

flattenSolution :: [Prop] -> [Prop]
flattenSolution x = concatMap computeSolution (pairwise x)

calculateSolution :: [Prop] -> Prop -> Bool
calculateSolution premises hyp | hyp `elem` (premises ++ flattenSolution premises) = True
                       | premises == (premises ++ flattenSolution premises)  = False
                       | otherwise = calculateSolution (premises ++ flattenSolution premises) hyp
