module Cube where

import Test.QuickCheck
import Data.List


data Cube = Cube { corners :: [Corner] }
	deriving (Show, Eq)

data Corner = Corner { corner :: ([Color],Int) }
	deriving (Show,Eq)


--Color corresponding to side: White(D), Yellow(U), Red(F), Orange(B), Green(R), Blue(L)
data Color = White | Yellow | Red | Orange | Blue | Green
	deriving (Eq, Show)

data Move = F| Fi | F2 | R | Ri | R2 | U | Ui | U2
	deriving (Show,Eq)

newSolvedCube :: Cube
newSolvedCube = Cube [Corner ([Red,Blue,Yellow],0), Corner ([Red,Yellow,Green],0), Corner ([Red,White,Blue],0),
						Corner ([Red,Green,White],0), Corner ([Orange,Yellow,Blue],0), Corner ([Orange,Green,Yellow],0)
						, Corner ([Orange,Blue,White],0), Corner ([Orange,White,Green],0)]

--Side order: Front(F), Back(B), Left(L), Right(R), Up(U), Down(D)
--Block order: Upper-Left, Upper-Right, Bottom Left, Bottom-Right
sides :: Cube -> [[Color]]
sides c = [extractSide c (fst cidAndSt) (snd cidAndSt) | cidAndSt <- 
	zip cornerIdList stateList]
	where
		extractSide :: Cube -> [Int] -> [Int] -> [Color]
		extractSide c cornerId state = [(fst (corner (corners c!!(fst cidAndSt)))) !! 
			(mod ((snd (corner (corners c!!(fst cidAndSt)))+(snd cidAndSt))) 3) | cidAndSt <- (zip cornerId state)]
		cornerIdList = [[0..3], [4..7], [4,0,6,2], [1,5,3,7], [4,5,0,1], [2,3,6,7]]
		stateList = [(replicate 4 0), (replicate 4 0), [2,1,1,2], [2,1,1,2], [1,2,2,1], [1,2,2,1]]

rotate :: Cube -> Move -> Cube
rotate c F 	= Cube ([((corners c)!!x)|x <- [2,0,3,1]]++(drop 4 (corners c)))
	where cs = corners c

rotate c Fi = Cube ([((corners c)!!x)|x <- [1,3,0,2]]++(drop 4 (corners c)))
rotate c F2 = rotate (rotate c F) F
rotate c U 	= update c [0,1,4,5] [4,0,5,1] [1,2,2,1]
rotate c Ui = update c [0,1,4,5] (reverse [4,0,5,1]) [2,1,1,2]
rotate c U2 = rotate (rotate c U) U
rotate c R 	= update c [1,3,5,7] [5,1,7,3] [2,1,1,2] 
rotate c Ri = update c [1,3,5,7] (reverse [5,1,7,3]) [1,2,2,1]
rotate c R2 = rotate (rotate c R) R

update :: Cube -> [Int] -> [Int] -> [Int] -> Cube
update c oldPos newPos states = updateCorners c (updateStates [((corners c)!!x)|x <- oldPos states) newPos

updateStates :: [Corner] -> [Int] -> [Corner] 
updateStates c states = [Corner ((fst (corner (snd x))),(mod ((snd (corner (snd x)))+(fst x)) 3))| x <- (zip states c)]  

updateCorners :: Cube -> [Corner] -> [Int] -> Cube
updateCorners  c [] [] = c
updateCorners  cube (c:cs) (p:ps) = updateCorners (Cube ((corners cube)!!=(p,c))) cs ps

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] 		_ 	= []
(!!=) (x:xs) (0,a) 	= a:xs
(!!=) (x:xs) (i,a)
	| i < 0 	= error "Invalid index" 
	| otherwise = x:(xs !!= (i-1,a))


--Check if the cube is solved by checking that each side only has one color
isSolved :: Cube -> Bool
isSolved c = all (==1) [length (groupBy (==) side) | side <- (sides c)]


