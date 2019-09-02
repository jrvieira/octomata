module Main where

import Zero
import Color
--import System.Console.ANSI
--import Control.Concurrent
import Data.List
import Data.Maybe
import Data.Map.Strict as Map (Map, fromList, (!), size)
import System.IO
import System.Environment
import System.Directory
import Codec.Picture
--import Debug.Trace

data State = O | I deriving (Eq, Show)
data Rel = N | W | E | S | NW | NE | SW | SE deriving Eq
type Pos = (Int,Int)
type Frame = Map Pos State

main :: IO ()
main = mapM_ draw $ buffer seed

seed :: Frame
seed = Map.fromList [((0,0),I)]

buffer :: Frame -> [Frame]
buffer = iterate next

(><) :: Frame -> Pos -> State
f >< p
   | x >= s || y >= s = O
   | otherwise        = f ! (x,y)
   where
      s     = side f
      (x,y) = (ord !! 1,ord !! 0)
      ord   = sort $ reflect <$> [fst p,snd p]
      reflect i
         | i < 0     = - i - 1
         | otherwise = i

side :: Frame -> Int
side f = tri 0 1 (size f)
   where
      tri acc i n
         | acc + i == n = i
         | otherwise    = tri (acc+i) (i+1) n

-- generation

next :: Frame -> Frame
next f = Map.fromList $ automaton <$> [(x,y) | x <- [0..i] , y <- [0..i] , x <= i , y <= x]
   where
      i = side f
      automaton :: Pos -> (Pos, State)
      automaton p = case f >< p of
         I -> if count I (adjacents f p) `elem` [       ] then (p,I) else (p,O)
         O -> if count I (adjacents f p) `elem` [2,3,4  ] then (p,I) else (p,O)

adjacents :: Frame -> Pos -> [State]
adjacents f p = ((f ><).(p #)) <$> [N,W,E,S,NW,NE,SW,SE]

(#) :: Pos -> Rel -> Pos
(#) p@(x,y) r
   | N  <- r = (x  ,y+1)
   | W  <- r = (x-1,y  )
   | E  <- r = (x+1,y  )
   | S  <- r = (x  ,y-1)
   | NW <- r = p # N # W
   | NE <- r = p # N # E
   | SW <- r = p # S # W
   | SE <- r = p # S # E

-- draw

black :: Pixel8
black = 0

white :: Pixel8
white = 255

pixel :: Frame -> Int -> Int -> Pixel8
pixel f x y
   | I <- f >< (x',y') = black
   | _ <- f >< (x',y') = white
   where
      s  = side f
      x' = x - s
      y' = y - s

draw :: Frame -> IO ()
draw f = do
   createDirectoryIfMissing True "io"
   done <- doesFileExist file
   if done then do
      putStrLn $ clr White (file ++ " skipped")
   else do
      savePngImage file $ ImageY8 $ generateImage (pixel f) w h
      putStrLn $ clr Green file
   where
      s = side f
      w = s * 2
      h = w
      file = "io/" ++ show (s - 1) ++ "rug.png"

