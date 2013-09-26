{-
Lucas Nunno
Spring 2013
CS 491: Advanced Functional Programming
University of New Mexico
Snake Game
-}
import Data.Array
import Data.List
import qualified Data.Sequence as S
import qualified Data.Foldable as Foldable
import Control.Concurrent
import System.IO
import System.Random
import System.Console.ANSI

type Coord = (Int,Int)
type Field = Array Coord Char
{-
Snake should always be initialized with at least one body segment.
-}
data Snake = Snake {
					snakeHead :: Coord,
					snakeTail :: Coord,
					snakeBody :: S.Seq Coord
					} deriving (Show)
					
data Direction = DLeft | DRight | DUp | DDown deriving (Show,Eq,Enum,Bounded)
data Game = Game {
					snake :: Snake,
					apples :: [Coord],
					field :: Field,
					growState :: Bool,
					direction :: Direction,
					score :: Int,
					timeDelay :: Int
				 } deriving (Show)
unpackGameState g = (snake g, apples g, field g, growState g, direction g, score g, timeDelay g)
			
moveSnake :: Direction -> Snake -> Snake			
moveSnake dir s = updatedSnake
	where 
		h@(yh,xh) = snakeHead s
		sbody = snakeBody s
		updatedHead
			| dir == DLeft 		= (yh,xh-1)
			| dir == DRight 	= (yh,xh+1)
			| dir == DUp 		= (yh-1,xh)
			| dir == DDown		= (yh+1,xh)
		-- Tail is now the last segment of the body.
		updatedTail = head $ Foldable.toList $ S.take 1 sbody
		-- Update the body.
		bodyTemp = S.drop 1 sbody
		-- The previous head is now on the body.
		updatedBody = bodyTemp S.|> h
		updatedSnake = Snake updatedHead updatedTail updatedBody
		
			
{-
Growing the snake is the same as moving without updating the tail position.		
So simply add another tail segment to the body.
-}
growSnake :: Snake -> Snake
growSnake s = biggerSnake
	where
		t = snakeTail s
		sbody = snakeBody s
		biggerBody = t S.<| sbody
		biggerSnake = Snake (snakeHead s) (snakeTail s) biggerBody
		
initialScore = 0
numberOfApples = 25
numNewApples = 1
xMax = 80
yMax = 30
snakeChar = '1'
emptyChar = ' '
appleChar = '@'
		
main = do
	-- Remove buffering for input. Looks like it doesn't work for Windows.
	hSetBuffering stdin NoBuffering
	let field = empty2DArray xMax yMax
	randomCoordsStream <- randomCoords xMax yMax
	let randomApplePlacement = take numberOfApples randomCoordsStream
	let snake = Snake (0,2) (0,0) (S.fromList [(0,1)])
	let gameState = Game snake randomApplePlacement field False DDown initialScore delayTimeMicro
	timeLoop gameState 
	
randomCoords xMax yMax = do
		g <- newStdGen
		let xRs = randomRs (0,xMax) g :: [Int]
		let yRs = randomRs (0,yMax) g :: [Int]
		let stream = zip yRs xRs
		return stream
	
delayTimeMicro = 250000
gameSpeedupRate = 5000
speedThreshold = 50000
speedIncreaseChance = 10

timeLoop gameState = do
	-- unpackGameState g = (snake g, apples g, field g, growState g, direction g, score g)
	let (snake,apples,field,grow,moveDir,score,delayTimeMicro) = unpackGameState gameState
	keyInput <- getInputIfReady
	let nextDir 
		| keyInput == 'w' = DUp
		| keyInput == 'a' = DLeft
		| keyInput == 's' = DDown
		| keyInput == 'd' = DRight
		| otherwise = moveDir
    -- DEBUG print $ "Key Input is " ++ [keyInput]
	let newSnake = nextSnake nextDir grow snake
    -- See what the new snake's head is moving into.
	let nextMoveCoord = snakeHead newSnake
	let isDead = shouldDie field nextMoveCoord
	if isDead 
	then do
		putStrLn $ "Your snake has met its doom! Dare you try again?\nFINAL SCORE: " ++ (show score)
		threadDelay 650000
                input <- getChar 
		if input == 'y' then main
		else do
			putStrLn "Goodbye!"
			threadDelay 250000
	else do
		roll <- randomRIO(1,10) :: IO Int
		let speedup = (roll == 1)
		let isGrowing = (field ! nextMoveCoord) == appleChar
		randomCoordStream <- randomCoords xMax yMax
		let newAppleCoords = take numNewApples randomCoordStream
		let updatedApples 
			| isGrowing = (delete nextMoveCoord apples) ++ newAppleCoords
			| otherwise = apples
		let newField = refreshField field newSnake updatedApples
		let newScore = calculateNewScore isGrowing score (S.length $ snakeBody snake)
		clearScreen
		setCursorPosition 0 0
		print2DCharArray newField
		putStrLn $ "Score: " ++ (show newScore)
		threadDelay delayTimeMicro
		let modifiedTime = (delayTimeMicro - gameSpeedupRate)
		-- Only speedup on collection of apples.
		let newDelay 
			| (modifiedTime > speedThreshold) && speedup = modifiedTime
			| otherwise = delayTimeMicro
		let nextGameState = Game newSnake updatedApples newField isGrowing nextDir newScore newDelay
		timeLoop nextGameState

timePoints = 10
applePoints = 100
calculateNewScore appleCollected previousScore snakeBodyLength = newScore
	where
		newScore
			| appleCollected = previousScore + (applePoints + timePoints) * snakeBodyLength
			| otherwise = previousScore + timePoints * snakeBodyLength
		
	
shouldDie :: Array Coord Char -> Coord -> Bool
shouldDie field position = result
	where 
		result 
			| (not $ inRange (bounds field) position) = True
			| ((field ! position) == snakeChar) = True
			| otherwise = False
			
getInputIfReady = do
	ready <- hReady stdin
	if ready
		then hGetChar stdin
		else return ' '

bodyGrowth = 4	
nextSnake moveDir grow snake = newSnake 
	where
		newSnake
			| grow 		= moveSnake moveDir $ ((iterate growSnake snake) !! bodyGrowth)   
			| otherwise = moveSnake moveDir snake
	
-- refreshField :: Array -> Snake -> Array	
refreshField arr snake apples = newField // (snakeCoords ++ appleCoords)
	where
		((_,_),(ymax,xmax)) = bounds arr
		newField = empty2DArray xmax ymax
		snakeCoords = (zip (snakeToList snake) (repeat snakeChar))
		appleCoords = zip apples (repeat appleChar)
	
-- TODO Slow as shit.
printPretty2DCharArray arr = putPrettyStrLn $ unlines $ splitEveryN width $ concatMap (\x -> [x]) (elems arr)             
    where
        width = (1 + ) $ snd . snd $ (bounds arr)
		
putPrettyStrLn s = do
	mapM_ putPrettyChar s
	setSGR []

putPrettyChar c = do
	setSGR [SetColor Foreground Vivid (getColor c)]
	putChar c
	
getColor c 
	| c == emptyChar = Black
	| c == snakeChar = Green
	| c == appleChar = Red
	| otherwise = Black
		
-- Array Utility Functions -- 	
snakeToList s = snakeList
	where
		snakeList = (snakeTail s):((Foldable.toList (snakeBody s)) ++ [(snakeHead s)])
		
print2DCharArray arr = putStrLn $ unlines $ splitEveryN width $ concatMap (\x -> [x]) (elems arr)             
    where
        width = (1 + ) $ snd . snd $ (bounds arr)		

-- NOTE: Access the array returned as ! (y,x)
empty2DArray xMax yMax = array ((0,0),(yMax,xMax)) emptyAssoc
    where
        emptyAssoc = zip (xyCoords xMax yMax) (repeat emptyChar)
		
splitEveryN _ [] = []
splitEveryN n xs = ys : splitEveryN n zs 
    where (ys,zs) = splitAt n xs 

xyCoords xMax yMax = [(y,x) | y <- [0..yMax], x <- [0..xMax]]	
