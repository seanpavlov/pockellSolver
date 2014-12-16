module Cube where

import Control.Parallel
import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Char
import System.Random

-----------DATATYPES---------------------------------------------------------
--A Cube is composed of a list of corners.
data Cube = Cube { corners :: [Corner] }
	deriving (Show, Eq)

--A corner of a cube is represented by a list of colors 
--and their respictive faces.
data Corner = Corner { faceColors :: [(Color, Face)] }
	deriving (Show,Eq)

--Color corresponding to a face on a corner.
data Color = White | Yellow | Red | Orange | Blue | Green
	deriving (Eq, Show, Ord)

--A move is a way to rotate the cube.
data Move = F | Fi | F2 | R | Ri | R2 | U | Ui | U2
	deriving (Show,Eq)

--Different sides of the cube.
data Face = Ff | Bf | Lf | Rf | Uf | Df
	deriving(Show,Eq)

-----------FUNCTIONS---------------------------------------------------------
--Starts the program.
main :: IO ()
main = do 
		putStr "Input Cube: (s for shuffled cube, or colors W,Y,"
		putStrLn "B,G,R,O in order Front,Back,Left,Right,Up,Down"
		cs <- getLine 
		case cs of
			"s" -> do 
					putStrLn "Number of rotations for shuffle:"
					snrLine <- getLine
					let snr = read snrLine :: Integer
					g <- newStdGen
					putStr "Shuffle moves: "
					print (snd (shuffle g newSolvedCube snr))
					putStrLn "Solving..."
					print (snd (fromJust (solve (fst 
						(shuffle g newSolvedCube snr)))))
					putStrLn "Solution found! Moves: ^"
			_ -> do
					putStrLn "Solving..."
					print (snd (fromJust ((solve (charsToCube(charsToColor 
						(map toUpper cs)))))))
					putStrLn "Solution found! Moves: ^"

	where
		--Given	list of chars translates them into colors.
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
			| otherwise = error "Wrong input. Valid inputs are: W,Y,B,G,R,O."

		--Given a list of colors, translates them into a cube.
		charsToCube :: [Color] -> Cube
		charsToCube cList = do
			case (length cList) of
				24 -> do
					let c = Cube [Corner [((cList!!(fst x)),(snd x)) | 
							x <- zip (fst y) (snd y)] | 
							y <- zip cornersFromSides faces]
					case (isValidCube c) of
						True 	-> c
						False 	-> error "Not a valid cube."
				_ -> error "Incorrect number of colors."

--Gives a new solved cube with: White(Down), Yellow(Up), Red(Front), 
--Orange(Back), Green(Right), Blue(Left)
newSolvedCube :: Cube
newSolvedCube = Cube [Corner [(Red,Ff),(Blue,Lf),(Yellow,Uf)], 
					Corner [(Red,Ff),(Green,Rf),(Yellow,Uf)], 
					Corner [(Red,Ff),(Blue,Lf),(White,Df)],
					Corner [(Red,Ff),(Green,Rf),(White,Df)], 
					Corner [(Orange,Bf),(Blue,Lf),(Yellow,Uf)], 
					Corner [(Orange,Bf),(Green,Rf),(Yellow,Uf)],
					Corner [(Orange,Bf),(Blue,Lf),(White,Df)], 
					Corner [(Orange,Bf),(Green,Rf),(White,Df)]]

--Side order: Front(F), Back(B), Left(L), Right(R), Up(U), Down(D)
--Block order: Upper-Left, Upper-Right, Bottom Left, Bottom-Right
sides :: Cube -> [[Color]]
sides c = [[extractColor (faceColors ((corners c)!!x)) (fst idFace) | 
			x <- (snd idFace)] | idFace <- zip [Ff,Bf,Lf,Rf,Uf,Df] [[0..3], 
			[5,4,7,6], [4,0,6,2], [1,5,3,7], [4,5,0,1], [2,3,6,7]]]
	where 
		extractColor :: [(Color,Face)] -> Face -> Color
		extractColor [] _ = error "toooooo much painz"
		extractColor (c:cs) f
			| (snd c) == f 	= fst c
			| otherwise 	= extractColor cs f

--Rotates a cube depending on what move is given.
rotate :: Cube -> Move -> Cube
rotate c f
	| f == F 	= update c [0..3] [1,3,0,2] [[(Uf,Rf),(Lf,Uf)],
					[(Rf,Df),(Uf,Rf)],[(Lf,Uf),(Df,Lf)],[(Df,Lf),(Rf,Df)]]
	| f == Fi 	= update c [0..3] [2,0,3,1] [[(Lf,Df),(Uf,Lf)],
					[(Uf,Lf),(Rf,Uf)],[(Df,Rf),(Lf,Df)],[(Rf,Uf),(Df,Rf)]]
	| f == F2 	= update c [0..3] [3,2,1,0] [[(Lf,Rf),(Uf,Df)],
					[(Uf,Df),(Rf,Lf)],[(Df,Uf),(Lf,Rf)],[(Rf,Lf),(Df,Uf)]]
	| f == U 	= update c [0,1,4,5] [4,0,5,1] [[(Lf,Bf),(Ff,Lf)],
					[(Ff,Lf),(Rf,Ff)],[(Bf,Rf),(Lf,Bf)],[(Rf,Ff),(Bf,Rf)]]
	| f == Ui 	= update c [0,1,4,5] [1,5,0,4] [[(Ff,Rf),(Lf,Ff)],
					[(Rf,Bf),(Ff,Rf)],[(Lf,Ff),(Bf,Lf)],[(Bf,Lf),(Rf,Bf)]]
	| f == U2 	= update c [0,1,4,5] [5,4,1,0] [[(Ff,Bf),(Lf,Rf)],
					[(Rf,Lf),(Ff,Bf)],[(Lf,Rf),(Bf,Ff)],[(Bf,Ff),(Rf,Lf)]]
	| f == R 	= update c [1,3,5,7] [5,1,7,3] [[(Uf,Bf),(Ff,Uf)],
					[(Ff,Uf),(Df,Ff)],[(Bf,Df),(Uf,Bf)],[(Df,Ff),(Bf,Df)]]
	| f == Ri 	= update c [1,3,5,7] [3,7,1,5] [[(Ff,Df),(Uf,Ff)],
					[(Df,Bf),(Ff,Df)],[(Uf,Ff),(Bf,Uf)],[(Bf,Uf),(Df,Bf)]]
	| f == R2 	= update c [1,3,5,7] [7,5,3,1] [[(Ff,Bf),(Uf,Df)],
					[(Df,Uf),(Ff,Bf)],[(Uf,Df),(Bf,Ff)],[(Bf,Ff),(Df,Uf)]]
	where
		update :: Cube -> [Int] -> [Int] -> [[(Face,Face)]] -> Cube
		update c oldPos newPos fs = updateCorners c (updateFaces 
			[((corners c)!!x)|x <- oldPos] fs) newPos
			
		updateFaces :: [Corner] -> [[(Face,Face)]] -> [Corner] 
		updateFaces c fs = [Corner (updateFace 
			(faceColors (fst x)) (snd x)) | x <- (zip c fs)]
			
		updateFace :: [(Color,Face)] -> [(Face,Face)] -> [(Color,Face)]
		updateFace c [] = c
		updateFace c (ff:ffs) = updateFace (updateSide c ff) ffs

		updateSide :: [(Color,Face)] -> (Face,Face) -> [(Color,Face)]
		updateSide (c:cs) ff
			| not ((fst ff) == (snd c)) = c:(updateSide cs ff)
			| otherwise					= ((fst c),(snd ff)):cs   

		updateCorners :: Cube -> [Corner] -> [Int] -> Cube
		updateCorners  c [] [] = c
		updateCorners  cube (c:cs) (p:ps) = 
			updateCorners (Cube ((corners cube)!!=(p,c))) cs ps

		(!!=) :: [a] -> (Int,a) -> [a]
		(!!=) [] 		_ 	= []
		(!!=) (x:xs) (0,a) 	= a:xs
		(!!=) (x:xs) (i,a)
			| i < 0 	= error "Invalid index" 
			| otherwise = x:(xs !!= (i-1,a))


--Check if the cube is solved by checking that each side only has one color
isSolved :: Cube -> Bool
isSolved c = all (==1) [length (groupBy (==) side) | side <- (sides c)]

--Takes a StdGen, cube and number of random moves it should do
--and shuffles it. Returns a tuple with the shuffled cube
--and a list of moves it made to shuffle
shuffle :: StdGen -> Cube -> Integer -> (Cube, [Move])
shuffle g c i = shuffle' g c i []
	where
		shuffle' :: StdGen -> Cube -> Integer -> [Move] -> (Cube, [Move])
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

--Rotates a cube all possible ways in all states recursievly
--and returns the first occurence of a solved cube.			
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


--Checks whether a cube is valid.
--(Correct number of colors, faces on edges and number of corners)
isValidCube :: Cube -> Bool
isValidCube c = and [(correctColors c),
	(correctNbrOfCorners c),(correctFaces c)]
	where
		correctColors :: Cube -> Bool
		correctColors c = all (\x -> (length x)==4) 
			(groupBy (==) (sort (foldr (++) [] (sides c))))
		correctNbrOfCorners :: Cube -> Bool
		correctNbrOfCorners c = ((length (corners c)) == 8)
		correctFaces :: Cube -> Bool
		correctFaces c = and [checkCorner ((faceColors (fst x)),(snd x)) | 
			x <- (zip (corners c) faces)]
			where 
				checkCorner :: ([(Color,Face)],[Face]) -> Bool
				checkCorner (_,[]) = True
				checkCorner ((c:cs),fs) = (elem (snd c) fs) &&
					(checkCorner (cs,(delete (snd c) fs)))

cornersFromSides = [[0,9,18],[1,12,19],[2,11,20],[3,14,21],
					[5,8,16],[4,13,17],[7,10,22],[6,15,23]]
faces = [[Ff,Lf,Uf],[Ff,Rf,Uf],[Ff,Lf,Df],[Ff,Rf,Df],
					[Bf,Lf,Uf],[Bf,Rf,Uf],[Bf,Lf,Df],[Bf,Rf,Df]]

-----------PROPS-------------------------------------------------------------

prop_sides :: Cube -> Bool
prop_sides c = all (\x -> (length x) == 4) (sides c)

prop_solve :: Cube -> Bool
prop_solve c = isJust (solve c)

-- prop_shuffle1 :: StdGen -> Integer -> Bool
-- prop_shuffle1 std (Positive i) c1 c2 = (shuffle std newSolvedCube i) != (shuffle std newSolvedCube i)

prop_shuffle2 :: StdGen -> Integer -> Property
prop_shuffle2 std i = i>1 ==> not (isSolved (fst(shuffle std newSolvedCube i)))

prop_validCube :: Cube -> Bool
prop_validCube = isValidCube

-- moves generates an arbitrary cell in a Sudoku
color :: Gen (Color)
color = frequency [(1, return Red), (1, return Orange), (1, return Blue),
	(1, return Green), (1, return Yellow), (1, return White)]

-- an instance for generating Arbitrary Cubes
instance Arbitrary Cube where
	arbitrary =
		do 	randomColors <- sequence [ sequence [ color | j <- [1..3] ] | i <- [1..8] ]
			return (Cube [Corner [ ((fst y), (snd y)) | y <- zip (fst x) (snd x)] | x <- zip randomColors faces])
 

{--- an instance for generating Arbitrary Cubes
instance Arbitrary Cube where
	arbitrary =
  		do 	g <- newStdGen
  			let (n', g') = (randomR (1, 100) g)
  			return (fst (shuffle g newSolvedCube n'))-}
					

















