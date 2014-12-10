module Cube where

import Test.QuickCheck

--Side order: Front(F), Back(B), Left(L), Right(R), Up(U), Down(D)
--Block order: Upper-Left, Upper-Right, Bottom Left, Bottom-Right
data Cube = Cube { sides :: [[Color]] }
	deriving (Show, Eq)

--Color corresponding to side: White(D), Yellow(U), Red(F), Orange(B), Green(R), Blue(L)
data Color = White | Yellow | Red | Orange | Blue | Green
	deriving (Eq, Show)

newSolvedCube :: Cube
newSolvedCube = Cube [replicate 4 c | c <- [Red, Orange, Blue, Green, Yellow, White]]