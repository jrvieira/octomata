module Main where

import Zero
import Color
--import System.Console.ANSI
--import Control.Concurrent
import Data.List
import Data.Maybe
import Data.Map.Strict as Map ( Map , fromList , (!) , size )
import System.IO
import System.Environment
import System.Directory
import Codec.Picture
--import Debug.Trace

type State = Bool
data Rel = N | W | E | S | NW | NE | SW | SE deriving Eq
type Pos = (Int,Int)
type Frame = Map Pos State

dir = "io"

main :: IO ()
main = do
   createDirectoryIfMissing False dir
   removeDirectoryRecursive dir
   createDirectory dir
   mapM_ draw $ buffer seed

seed :: Frame
seed = Map.fromList [((0,0),True)]

buffer :: Frame -> [Frame]
buffer = iterate step

(><) :: Frame -> Pos -> State
f >< p
   | x >= s || y >= s = False
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

step :: Frame -> Frame
step f = Map.fromList $ automaton <$> [(x,y) | x <- [0..i] , y <- [0..x]]
   where
   i = side f
   automaton :: Pos -> (Pos,State)
   automaton p
      | f >< p    = (p , a `elem` [ ])      -- S
      | otherwise = (p , a `elem` [2,3,4])  -- B
      where
      a = count True (adjacents f p)

adjacents :: Frame -> Pos -> [State]
adjacents f p = ((f ><) . (p #)) <$> [N,W,E,S,NW,NE,SW,SE]

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
   | f >< (x',y') = black
   | otherwise    = white
   where
   s  = side f
   x' = x - s
   y' = y - s

draw :: Frame -> IO ()
draw f = do
   savePngImage file $ ImageY8 $ generateImage (pixel f) w h
   putStrLn $ clr Green file
   where
   s = side f
   w = s * 2
   h = w
   file = dir ++ "/" ++ show (s - 1) ++ "rug.png"

