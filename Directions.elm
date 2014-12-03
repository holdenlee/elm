module Directions where

import Keyboard
import Set

type Dir = Int

-- arrows = directions 38 40 37 39
-- Inputs
upArrow : Dir
upArrow = 38

downArrow : Dir
downArrow = 40

leftArrow : Dir
leftArrow = 37

rightArrow : Dir
rightArrow = 39

dirs : Set.Set Dir
dirs = Set.fromList [upArrow,downArrow,leftArrow,rightArrow]


upFrom : (Int,Int) -> (Int,Int)
upFrom (x,y) = (x,y+1)

leftFrom : (Int,Int) -> (Int,Int)
leftFrom (x,y) = (x-1,y)

rightFrom : (Int,Int) -> (Int,Int)
rightFrom (x,y) = (x+1,y)

downFrom : (Int,Int) -> (Int,Int)
downFrom (x,y) = (x,y-1)

dirFrom : Dir -> (Int,Int) -> (Int,Int)
dirFrom dir (x,y) = if | dir==downArrow -> downFrom (x,y)
                       | dir==upArrow -> upFrom (x,y)
                       | dir==leftArrow -> leftFrom (x,y)
                       | dir==rightArrow -> rightFrom (x,y)
                       | otherwise -> (x,y)
