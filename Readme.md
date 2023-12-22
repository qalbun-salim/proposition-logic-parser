# Propositional Logic Solver with Haskell

This program purpose to solve a propositional logic problem by applying inference rules to the given premises using functional programming approach. The inference rules are made as functions then used by the calculate function to determine whether the given hypotesis is valid for the given premises. 

## Installation
This solver can be run interactively using GHCI with tests that can be run with cabal. The installation guide are as follows:
1. Install [GHCup](https://www.haskell.org/ghcup/).
2. With GHCup, the installation of GHCI and cabal should have been included. To verify, you can run `ghci --version` and `cabal --version`. If cabal hasn't been installed, you can do it via command line by running `ghcup install cabal`.

## Usage

Start the program.

```
cd app 
ghci
```
Make sure you have ghci installed so you can run our haskell program. Then load the Algorithm module.  

```
:l Algorithm.hs
```

After the program started you can test your hypothesis using the `calculateSolution` function.

```
calculateSolution {premises} {hypothesis}
```
the premises is a list of propotion that looks like this [prop1, prop2, prop3, ...] the hypothesis is also a prop type, explained in the next section.

Example: 
```
calculateSolution [Var "P" :->: Var "Q", Var "Q" :->: Var "R", Var "P"](Var "R")
-- return: True
```
```
calculateSolution [Var "P" :->: Var "Q", Var "Q" :->: Var "R", Var "P"](Var "M")
-- return: False
```

## Prop Datatype

Each prop can be defined as `Var "name"` ex: `Var "Q"`. More about example:  

- single prop: `Var "name"` ex: `Var "Q"`
- negation prop: `Not (Var "name")` ex: `Not(Var "Q)"`
- And prop: `Var "name" :&: Var "name"` ex: `Var "P" :&: Var "Q"`
- Or Prop: `Var "name" :|: Var "name"` ex: `Var "P" :|: Var "Q"`
- Inference Prop : `Var "name" :->: Var "name"` ex:`Var "P" :->: Var "Q"`
- Combination: `Not(Var "P") :->: Var "Q"`

## Inference Function
The inference rules are available in the `Rules.hs` module and can be accessed independetly. Available rules:  
- modusPonens
- modusTollens
- silogismeHipotetik
- silogismeDisjungtif
- simplifikasi
- resolusi

## For modifications
To add more rule to the computeSolution algorithm you can add the new functions to either `solverOne `, `solverTwo`, or `solverThree` base on the parameter it takes in the `app/Algorithm.hs`. 

example:
```
solverOne [implifikasi, yourNewFunction]
```