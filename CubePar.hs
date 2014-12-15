module Cube where

import Control.Parallel
import Test.QuickCheck
import Data.List
import Data.Maybe
import System.Random


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
sides c = [[extractColor (corner ((corners c)!!x)) (fst idFace) | x <- (snd idFace)] | idFace <- zip [Ff,Bf,Lf,Rf,Uf,Df] [[0..3], [5,4,7,6], [4,0,6,2], [1,5,3,7], [4,5,0,1], [2,3,6,7]]]
	where 
		extractColor :: [(Color,Face)] -> Face -> Color
		extractColor [] _ = error "toooooo much painz"
		extractColor (c:cs) f
			| (snd c) == f 	= fst c
			| otherwise 	= extractColor cs f


rotate :: Cube -> Move -> Cube
rotate c F 	= update c [0..3] [1,3,0,2] [[(Uf,Rf),(Lf,Uf)],[(Rf,Df),(Uf,Rf)],[(Lf,Uf),(Df,Lf)],[(Df,Lf),(Rf,Df)]]
rotate c Fi = update c [0..3] [2,0,3,1] [[(Lf,Df),(Uf,Lf)],[(Uf,Lf),(Rf,Uf)],[(Df,Rf),(Lf,Df)],[(Rf,Uf),(Df,Rf)]]
rotate c F2 = update c [0..3] [3,2,1,0] [[(Lf,Rf),(Uf,Df)],[(Uf,Df),(Rf,Lf)],[(Df,Uf),(Lf,Rf)],[(Rf,Lf),(Df,Uf)]]
rotate c U 	= update c [0,1,4,5] [4,0,5,1] [[(Lf,Bf),(Ff,Lf)],[(Ff,Lf),(Rf,Ff)],[(Bf,Rf),(Lf,Bf)],[(Rf,Ff),(Bf,Rf)]]
rotate c Ui = update c [0,1,4,5] [1,5,0,4] [[(Ff,Rf),(Lf,Ff)],[(Rf,Bf),(Ff,Rf)],[(Lf,Ff),(Bf,Lf)],[(Bf,Lf),(Rf,Bf)]]
rotate c U2 = update c [0,1,4,5] [5,4,1,0] [[(Ff,Bf),(Lf,Rf)],[(Rf,Lf),(Ff,Bf)],[(Lf,Rf),(Bf,Ff)],[(Bf,Ff),(Rf,Lf)]]
rotate c R 	= update c [1,3,5,7] [5,1,7,3] [[(Uf,Bf),(Ff,Uf)],[(Ff,Uf),(Df,Ff)],[(Bf,Df),(Uf,Bf)],[(Df,Ff),(Bf,Df)]]
rotate c Ri = update c [1,3,5,7] [3,7,1,5] [[(Ff,Df),(Uf,Ff)],[(Df,Bf),(Ff,Df)],[(Uf,Ff),(Bf,Uf)],[(Bf,Uf),(Df,Bf)]]
rotate c R2 = update c [1,3,5,7] [7,5,3,1] [[(Ff,Bf),(Uf,Df)],[(Df,Uf),(Ff,Bf)],[(Uf,Df),(Bf,Ff)],[(Bf,Ff),(Df,Uf)]]

update :: Cube -> [Int] -> [Int] -> [[(Face,Face)]] -> Cube
update c oldPos newPos fs = updateCorners c (updateFaces [((corners c)!!x)|x <- oldPos] fs) newPos

updateFaces :: [Corner] -> [[(Face,Face)]] -> [Corner] 
updateFaces c fs = [Corner (updateFace (corner (fst x)) (snd x)) | x <- (zip c fs)]
	where
		updateFace :: [(Color,Face)] -> [(Face,Face)] -> [(Color,Face)]
		updateFace c [] = c
		updateFace c (ff:ffs) = updateFace (updateSide c ff) ffs

		updateSide :: [(Color,Face)] -> (Face,Face) -> [(Color,Face)]
		updateSide (c:cs) ff
			| not ((fst ff) == (snd c)) = c:(updateSide cs ff)
			| otherwise					= ((fst c),(snd ff)):cs   


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

--Takes a StdGen, cube and number of random moves it should do and shuffles it
--Returns a tuple with the shuffled cube and a list of moves it made to shuffle
shuffle :: StdGen -> Cube -> Int -> (Cube, [Move])
shuffle g c i = shuffle' g c i []
	where
		shuffle' :: StdGen -> Cube -> Int -> [Move] -> (Cube, [Move])
		shuffle' g c i ms
			| i == 0 = (c, ms)
			| otherwise = shuffle' g' c' (i-1) (ms ++ [m'])
			where
				(n', g') = (randomR (0, 8) g)
				(c', m') = rotateSide c n'

				rotateSide :: Cube -> Int -> (Cube, Move)
				rotateSide c 0 = ((rotate c F), F)
				rotateSide c 1 = ((rotate c Fi), Fi)
				rotateSide c 2 = ((rotate c F2), F2)
				rotateSide c 3 = ((rotate c U), U)
				rotateSide c 4 = ((rotate c Ui), Ui)
				rotateSide c 5 = ((rotate c U2), U2)
				rotateSide c 6 = ((rotate c R), R)
				rotateSide c 7 = ((rotate c Ri), Ri)
				rotateSide c 8 = ((rotate c R2), R2)
				
solve :: Cube -> Maybe (Cube,[Move])
solve c = solve'' (c,[]) moveList
	where
		solve' :: (Cube,[Move]) -> Move -> Maybe (Cube,[Move])
		solve' cm m
			| (length (snd cm))>11	= Nothing
			| isSolved (fst cm) 	= Just ((fst cm),(snd cm))
			| any (==m) [F,Fi,F2] 	= solve'' cm (moveList\\[F,Fi,F2])
			| any (==m) [U,Ui,U2] 	= solve'' cm (moveList\\[U,Ui,U2])
			| any (==m) [R,Ri,R2] 	= solve'' cm (moveList\\[R,Ri,R2])
		moveList = [F,Fi,F2,U,Ui,U2,R,Ri,R2]
		solve'' :: (Cube,[Move]) -> [Move] -> Maybe (Cube,[Move])
		solve'' cm (m:[]) 	= solve' ((rotate (fst cm) m),(snd cm)++[m]) m 
		solve'' cm (m:ms) 	= do
								let k = solve' ((rotate (fst cm) m),(snd cm)++[m]) m 
								case k of
									Nothing -> solve'' cm ms
									Just _ 	-> k


sortCM (c1,m1) (c2,m2)
	| (length m1) <= (length m2) 	= GT
	| (length m1) > (length m2) 	= LT

solveInputCube = undefined

main :: IO ()
main = do 
		putStrLn "Input Cube:"
		cs <- getLine
		putStr "Solution found! Moves:"
		print (snd (fromJust ((solve (charsToCube(charsToColor cs))))))
		
charsToColor :: [Char] -> [Color]
charsToColor [] = []
charsToColor (c:cs)
	| c == ' ' = charsToColor cs
	| c == 'W' = White:charsToColor cs
	| c == 'Y' = Yellow:charsToColor cs
	| c == 'B' = Blue:charsToColor cs
	| c == 'G' = Green:charsToColor cs
	| c == 'R' = Red:charsToColor cs
	| c == 'O' = Orange:charsToColor cs

charsToCube :: [Color] -> Cube
charsToCube cList = Cube [Corner [((cList!!(fst x)),(snd x)) | x <- zip (fst y) (snd y)] | y <- zip cornersFromSides faces]


cornersFromSides = [[0,9,18],[1,12,19],[2,11,20],[3,14,21],[5,8,16],[4,13,17],[7,10,22],[6,15,23]]
faces = [[Ff,Lf,Uf],[Ff,Rf,Uf],[Ff,Lf,Df],[Ff,Rf,Df],[Bf,Lf,Uf],[Bf,Rf,Uf],[Bf,Lf,Df],[Bf,Rf,Df]]




















