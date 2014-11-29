module WorldView (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe (..)
import List
import Set
--user-defined modules
import Utilities (..)
import MDict (..)
import Directions (..)
import Actor (..)
import World (..)
import Action (..)
import Input (..)
import World2 (..)

ht:Int
ht = 15

wd:Int
wd = 15

pixels:Int
pixels = 32

sidebarL : Int
sidebarL = 50


hp:Int
hp = (ht*pixels)

wp:Int
wp = (wd*pixels)

--playerDraw

--the window to display
viewCoord:World2 -> (Int,Int,Int,Int)
viewCoord w = (0,0,14,14)

--given the width w, height h, (x,y) where the elements should be situated, the elements, creates an element combining all of these.
collageAbs : Int -> Int -> [(Int,Int)] -> [Element] -> Element
collageAbs w h tuples elems = 
    collage w h (zipMap (\(x,y) -> \elem -> move ( (-(toFloat w)/2 + (toFloat (widthOf elem))/2 + toFloat x),  (-(toFloat h)/2 + (toFloat (heightOf elem))/2 + toFloat y)) (toForm elem)) tuples elems)

display : World2 -> Element
display w =
   let 
       (x0,y0,x1,y1) = viewCoord w
       coordList = listprod (range x0 x1) (range y0 y1)
       -- a list of (x,y),actor@(x,y)
       drawList:[((Int,Int),Int)]
       drawList = concat (List.map (\(x,y) -> List.map (\a -> ((x,y), a)) (Set.toList (mget (x,y) w.alocs))) coordList)
       -- the coordinates to draw
       drawCoords = List.map (\(x,y) -> pixels `smult` x) drawList
       drawPics = List.map (\(t,a) -> drawActor (getActor a w) t w) drawList
   in
       collageAbs (wp+sidebarL) hp ((0,0)::(480,450)::drawCoords)
            ((image 480 480 "bg.gif")::(plainText w.text)::drawPics)
