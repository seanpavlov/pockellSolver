module Cube where

import Test.QuickCheck
import Data.List


data Cube = Cube { corners :: [Corner] }
	deriving (Show, Eq)

data Corner = Corner { corner :: [(Color, Face)] }
	deriving (Show,Eq)


--Color corresponding to side: White(D), Yellow(U), Red(F), Orange(B), Green(R), Blue(L)
data Color = White | Yellow | Red | Orange | Blue | Green
	deriving (Eq, Show)

data Move = F | Fi | F2 | R | Ri | R2 | U | Ui | U2
	deriving (Show,Eq)

data Face = Ff | Bf | Lf | Rf | Uf | Df
	deriving(Show,Eq)

newSolvedCube :: Cube
newSolvedCube = Cube [Corner [(Red,Ff),(Blue,Lf),(Yellow,Uf)], Corner [(Red,Ff),(Green,Rf),(Yellow,Uf)], Corner [(Red,Ff),(Blue,Lf),(White,Df)],
						Corner [(Red,Ff),(Green,Rf),(White,Df)], Corner [(Orange,Bf),(Blue,Lf),(Yellow,Uf)], Corner [(Orange,Bf),(Green,Rf),(Yellow,Uf)]
						, Corner [(Orange,Bf),(Blue,Lf),(White,Df)], Corner [(Orange,Bf),(Green,Rf),(White,Df)]]

--Side order: Front(F), Back(B), Left(L), Right(R), Up(U), Down(D)
--Block order: Upper-Left, Upper-Right, Bottom Left, Bottom-Right
sides :: Cube -> [[Color]]
sides c = [[extractColor (corner ((corners c)!!x)) (fst idFace) | x <- (snd idFace)] | idFace <- zip [Ff,Bf,Lf,Rf,Uf,Df] [[0..3], [4..7], [4,0,6,2], [1,5,3,7], [4,5,0,1], [2,3,6,7]]]
	where 
		extractColor :: [(Color,Face)] -> Face -> Color
		extractColor [] _ = error "toooooo much painz"
		extractColor (c:cs) f
			| (snd c) == f 	= fst c
			| otherwise 	= extractColor cs f    


rotate :: Cube -> Move -> Cube
rotate c F 	= Cube ([((corners c)!!x)|x <- [2,0,3,1]]++(drop 4 (corners c)))
rotate c Fi = Cube ([((corners c)!!x)|x <- [1,3,0,2]]++(drop 4 (corners c)))
rotate c F2 = rotate (rotate c F) F
rotate c U 	= update c [0,1,4,5] [4,0,5,1] [2,1,1,2]
rotate c Ui = update c [0,1,4,5] (reverse [4,0,5,1]) [(-2),(-1),(-1),(-2)]
rotate c U2 = rotate (rotate c U) U
rotate c R 	= update c [1,3,5,7] [5,1,7,3] [2,1,1,2] 
rotate c Ri = update c [1,3,5,7] (reverse [5,1,7,3]) [(-1),(-2),(-2),(-1)]
rotate c R2 = rotate (rotate c R) R

update :: Cube -> [Int] -> [Int] -> [Int] -> Cube
update c oldPos newPos states = updateCorners c (updateStates [((corners c)!!x)|x <- oldPos] states) newPos

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


