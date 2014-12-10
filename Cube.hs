module Cube where

import Test.QuickCheck


data Cube = Cube { corners :: [Corner] }
	deriving (Show, Eq)

data Corner = Corner ([Color],Int)
	deriving (Show,Eq)


--Color corresponding to side: White(D), Yellow(U), Red(F), Orange(B), Green(R), Blue(L)
data Color = White | Yellow | Red | Orange | Blue | Green
	deriving (Eq, Show)

newSolvedCube :: Cube
newSolvedCube = Cube [Corner ([Red,Blue,Yellow],0), Corner ([Red,Yellow,Green],0), Corner ([Red,White,Blue],0),
						Corner ([Red,Green,White],0), Corner ([Orange,Green,Yellow],0), Corner ([Orange,Yellow,Blue],0)
						, Corner ([Orange,White,Green],0), Corner ([Orange,Blue,White],0)]

--Side order: Front(F), Back(B), Left(L), Right(R), Up(U), Down(D)
--Block order: Upper-Left, Upper-Right, Bottom Left, Bottom-Right
sides :: Cube -> [[Color]]
sides c = undefined
