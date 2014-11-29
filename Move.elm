module Move (..) where

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
import WorldView (..)

playerDraw:DrawActor
playerDraw = simpleDraw (croppedImage (60,0) 30 30 "/iceblox.gif")
--(image 32 32 "/player.jpg")

player:Actor
player = nullActor |> setType "player" |> setLoc (0,0)

--playerAction:Action
--playerAction = seqActions [simpleAction (\inp -> (\a -> (\w -> setLoc (tryMove w (a.locs!0) (getOneKey inp.keys)) a))), messageAction (\a -> show (a.locs!0))]
playerAction:Action
playerAction = seqActions [face, moveIn, messageAction (\a -> show (a.locs!0))]

worldStart:Int -> Int -> World2
worldStart x y = (emptyWorld2 x y) 
               |> (addActor player) 
               |> addType "player" playerAction nullReaction playerDraw

worldState = foldp stepWorld (worldStart wd ht) input

main = lift display worldState
