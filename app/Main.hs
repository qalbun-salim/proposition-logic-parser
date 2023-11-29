{-# LANGUAGE BlockArguments #-}
-- This is GHC Language Extension more info could be read here: https://wiki.haskell.org/Language_extensions & https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/intro.html
-- Specifically BlockArguments allow function to take block as argument, more info could be read here: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html

module Main where

import Test.Hspec
import Test.HUnit.Base

import Algorithm
import Rules

main :: IO ()
main = hspec do
        describe "Test individual rules" do
            it "Modus Ponens Rule OK" do 
                calculateSolution [Var "P" :->: Var "Q", Var "P"](Var "Q") `shouldBe` True
            it "Modus Ponens Rule Not OK" do 
                calculateSolution [Var "P" :->: Var "Q", Not(Var "P")](Var "Q") `shouldBe` False

            it "Modus Tollens Rule OK" do
                calculateSolution [Not (Var "Q"), Var "P" :->: Var "Q" ](Not (Var "P")) `shouldBe` True
            it "Modus Tollens Rule Not OK" do
                calculateSolution [Not (Var "Q"), Var "P" :->: Var "Q" ](Var "P") `shouldBe` False
                

            it "Silogisme Hipotetik Rule Ok" do
                calculateSolution [Var "P",Var "P" :->: Var "Q", Var "Q":->:Var"R"](Var "R") `shouldBe` True
            it "Silogisme Hipotetik Rule Not Ok" do
                calculateSolution [Var "P",Var "P" :->: Var "Q", Var "Q":->:Var"R"](Var "Q" :->: Var "P") `shouldBe` False


            it "Silogisme Disjungtif Rule Ok" do
                calculateSolution [Var "P" :|: Var "Q",Not (Var "P")](Var "Q") `shouldBe` True
            it "Silogisme Disjungtif Rule Not Ok" do
                calculateSolution [Var "P" :|: Var "Q",Not (Var "P")](Var "P") `shouldBe` False
            
            it "Resolusi Rule  OK" do
                calculateSolution [Var "P" :|: Var "Q", Not(Var "P"):|: Var "R"](Var "Q" :|: Var "R") `shouldBe` True
            it "Resolusi Rule  Not OK" do
                calculateSolution [Var "P" :|: Var "Q", Not(Var "P"):|: Var "R"](Var "P" :|: Var "R") `shouldBe` False


