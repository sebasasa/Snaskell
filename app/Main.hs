module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char), Event(EventKey) )
import System.Random
import Data.List

-- TODO 
-- Acomodar la ventana para que no comience realmente desde cero (De esa forma arreglamos el error de rounding)
-- Implementar colision con sigo misma
-- Hacer una pantalla de inicio (Presiona cualquier tecla (W A S D) apara empezar , Pulsa esc para cerrar )
-- Hacer todo el codigo un poco mas limpio y modular 

windowWidth :: Int
windowWidth = 300

windowHeight :: Int
windowHeight = 300

windowDisplay :: Display
windowDisplay = InWindow "HaskellSnake" (windowWidth, windowHeight) (100, 100)

dirRight :: Integer
dirRight = 0
dirLeft :: Integer
dirLeft = 1
dirUp :: Integer
dirUp = 2
dirDown :: Integer
dirDown = 3

gridSize :: Float
gridSize = 10

data Game = Game
    { positionX :: [Float]
    , positionY :: [Float]
    , direction :: Integer
    , foodPositionX :: Float
    , foodPositionY :: Float
    , score :: Int
    }



randomSeed :: Int
randomSeed = 4

randomInts :: Int -> Int -> [Int]
randomInts n seed = take n $ unfoldr (Just . random) (mkStdGen seed)

range :: Int
range = (min windowHeight windowWidth)

toInt :: Float -> Int
toInt = round

clipToRange :: Int -> Int
clipToRange x = toInt((fromIntegral (mod x range) - (fromIntegral range / 2))/gridSize)

listOfRandom :: [Int]
listOfRandom = map clipToRange (randomInts 100 randomSeed)

initialGame :: Game
initialGame = Game {
    positionX = [5 * gridSize, 4 * gridSize, 3 * gridSize],
    positionY = [5 * gridSize, 5 * gridSize, 5 * gridSize],
    direction = dirRight,
    foodPositionX = 7 * gridSize,
    foodPositionY = 7 * gridSize,
    score = 0
}


render :: Game -> Picture

generateColor :: Float -> Float -> Picture -> Picture
generateColor index totalSize = color (makeColor 1 ((((totalSize - index)/totalSize))) 0 ((((totalSize - index)/totalSize) + 1)/2))

renderSegment :: Int -> (Float , (Float, Float)) -> Picture
renderSegment size tuple = translate (fst (snd tuple)) (snd (snd tuple)) $ generateColor (fst tuple) (fromIntegral size)  $ rectangleWire gridSize gridSize

renderSnake :: Game -> [Picture]
renderSnake gameState = map (renderSegment (length $ positionX gameState)) (zip [0..] (zip (positionX gameState) (positionY gameState)))

renderFood :: Game -> [Picture]
renderFood gameState = [translate (foodPositionX gameState) (foodPositionY gameState) $ color (light blue) $ rectangleWire gridSize gridSize]

renderGameViewport :: [Picture]
renderGameViewport = [color (dark white) $ rectangleWire (fromIntegral windowWidth + 1) (fromIntegral windowHeight + 1)]

render gameState =
    pictures $ renderSnake gameState ++ renderFood gameState ++ renderGameViewport



handleInput :: Event -> Game -> Game

handleInput (EventKey (Char 'w') _ _ _) game = game { direction = dirUp    }
handleInput (EventKey (Char 's') _ _ _) game = game { direction = dirDown  }
handleInput (EventKey (Char 'a') _ _ _) game = game { direction = dirLeft  }
handleInput (EventKey (Char 'd') _ _ _) game = game { direction = dirRight }
handleInput _ game = game


moveSnake :: Game -> Game
moveSnake game
  | direction game == dirRight = game
    { positionX = (head (positionX game) + gridSize) : init (positionX game)
    , positionY = (head (positionY game) + 0       ) : init (positionY game)
    }
  | direction game == dirLeft = game
    { positionX = (head (positionX game) - gridSize) : init (positionX game)
    , positionY = (head (positionY game) + 0       ) : init (positionY game)
    }
  | direction game == dirUp = game
    { positionY = (head (positionY game) + gridSize) : init (positionY game)
    , positionX = (head (positionX game) + 0       ) : init (positionX game)
    }
  | direction game == dirDown = game
    { positionY = (head (positionY game) - gridSize) : init (positionY game)
    , positionX = (head (positionX game) + 0       ) : init (positionX game)
    }
  | otherwise = game


warpBounds :: Game -> Game
warpBounds game
  | head (positionX game) >=  (fromIntegral windowWidth/2) = game{
    positionX = (-fromIntegral windowWidth/2) : tail (positionX game)}
  | head (positionX game) <= -(fromIntegral windowWidth/2) = game{
    positionX = (fromIntegral windowWidth/2) : tail (positionX game)}
  | head (positionY game) >=  (fromIntegral windowWidth/2) = game{
    positionY = (-fromIntegral windowWidth/2) : tail (positionY game)}
  | head (positionY game) <= -(fromIntegral windowWidth/2) = game{
    positionY = (fromIntegral windowWidth/2) : tail (positionY game)}
  | otherwise = game


infinity :: Float
infinity = 1/0

colideWithFood :: Game -> Game
colideWithFood game = if (head (positionX game) == foodPositionX game) && (head (positionY game) == foodPositionY game) then game{
  positionX = positionX game ++ [infinity],
  positionY = positionY game ++ [infinity],
  score = (score game) + 2,
  foodPositionX = (fromIntegral $ (listOfRandom!!mod (score game + 0) 100)) * gridSize,
  foodPositionY = (fromIntegral $ (listOfRandom!!mod (score game + 1) 100)) * gridSize

} else game

colideWithSelf :: Game -> Game
colideWithSelf game = game

nextFrame :: Float -> Game -> Game
nextFrame stepSize game = colideWithSelf $ colideWithFood $ warpBounds (moveSnake game)

main :: IO ()
main = do
    print listOfRandom
    play
        windowDisplay
        (makeColor 0 0.01 0.05 1)
        12
        initialGame
        render
        handleInput
        nextFrame



