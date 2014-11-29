module Directions where

import Keyboard
import Set

-- arrows = directions 38 40 37 39
-- Inputs
upArrow : Int
upArrow = 38

downArrow : Int
downArrow = 40

leftArrow : Int
leftArrow = 37

rightArrow : Int
rightArrow = 39

dirs : Set.Set Int
dirs = Set.fromList [upArrow,downArrow,leftArrow,rightArrow]


upFrom : (Int,Int) -> (Int,Int)
upFrom (x,y) = (x,y+1)

leftFrom : (Int,Int) -> (Int,Int)
leftFrom (x,y) = (x-1,y)

rightFrom : (Int,Int) -> (Int,Int)
rightFrom (x,y) = (x+1,y)

downFrom : (Int,Int) -> (Int,Int)
downFrom (x,y) = (x,y-1)

dirFrom : Int -> (Int,Int) -> (Int,Int)
dirFrom dir (x,y) = if | dir==downArrow -> downFrom (x,y)
                       | dir==upArrow -> upFrom (x,y)
                       | dir==leftArrow -> leftFrom (x,y)
                       | dir==rightArrow -> rightFrom (x,y)
                       | otherwise -> (x,y)
