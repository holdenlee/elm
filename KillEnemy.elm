module Enemy (..) where

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

player:Actor
player = nullActor |> setType "player" |> setLoc (0,0)

ppAction:Action
ppAction = seqActions [
            messageAction (\a -> "I'm at " ++ show (a.locs!0)),
            face,
            (moveIn .| makeActorIn (dirFromInp velocityDir))
           ]

ppFace: D.Dict Int Element
ppFace = faceDict (List.map (\t -> croppedImage t 30 30 "iceblox.gif") [(150,0),(60,0),(210,0),(120,30),(60,0)])

ppDraw:DrawActor
ppDraw = faceDraw ppFace

block:Actor
block = nullActor |> setType "block" 
--|> setLoc (5,5)

blockAction:Action
blockAction = seqActions [(inertia2 (allActorsSatisfy (\x -> getType x == "enemy")))
              , doForAll (\x -> make x die) (getSatisfyingAtSameSpace (\x -> getType x == "enemy"))]

blockDraw:DrawActor
blockDraw = simpleDraw (croppedImage (0,60) 30 30 "iceblox.gif")

enemy:Actor
enemy = nullActor |> setType "enemy" |> setLoc (14,14)

enemyAction = seqActions [foldl1 (flip (.|)) (List.map (moveInDir2 
                                (allActorsSatisfy (\x -> getType x == "player")))
                                [downArrow,leftArrow])
-- ,
                          , makeRel getPlayerAtSameSpace die]

enemyDraw = simpleDraw (croppedImage (60,120) 30 30 "iceblox.gif")

worldStart:Int -> Int -> World2
worldStart x y = (emptyWorld2 x y) 
               |> (addActor player) 
               |> addType "player" ppAction nullReaction ppDraw
               |> (addActorAtLocs block [(5,5),(6,6),(7,7),(5,7)])
               |> addType "block" blockAction nullReaction blockDraw
               |> addActor enemy
               |> addType "enemy" enemyAction nullReaction enemyDraw
--(addActor block)

worldState = foldp stepWorld (worldStart 15 15) input

main = lift defaultDisplay worldState
