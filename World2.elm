module World2 (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe (..)
import List
import Set
import Array as A
--user-defined modules
import Utilities (..)
import MDict (..)
import Directions (..)
import Actor (..)
import World (..)
import Action (..)
import Input (..)

type DrawActor = (Actor -> (Int,Int) -> Element)
--D.Dict String (Actor -> (Int,Int) -> Element)

type World2 = World {actions: D.Dict String Action, reactions: D.Dict String Reaction, draws: D.Dict String DrawActor}
--todo: change the last one to Element -> Element for overlay?

emptyWorld2:Int -> Int -> World2
emptyWorld2 x y = 
    let ew = emptyWorld x y 
    in {l=x, h=y, adict=D.empty, ilist=A.empty, alocs=D.empty, text="", curId = 0, actions = D.empty, reactions = D.empty, draws = D.empty}
--{ew | actions = D.empty, reactions = D.empty, draw = D.empty}

drawActor: Actor -> (Int,Int) -> World2 -> Element
drawActor a (x,y) w = (getD (getType a) (\_ _ -> empty) w.draws) a (x,y)

stepWorld : Input -> World2 -> World2
stepWorld inp w = 
    let 
        w00 = {w - actions}
        w01 = {w00 - reactions}
        w0 = {w01 - draws | text = ""}
        (_,w2) = while 
                   (\(i,wo)-> (i<(A.length wo.ilist))) 
                   (0,w0) 
                   (\(i,wo) -> 
                       let a = getActor (A.getOrFail i wo.ilist) wo
                       in (i+1, (maybe wo (\action -> action inp a wo) (D.get (getType a) w.actions))))
        w20 = {w2 | actions = w.actions}
        w21 = {w20 | reactions = w.reactions}
    in {w21 | draws = w.draws}
--    in {w2 | actions =  w.actions, reactions = w.reactions, draw = w.draw}
--haven't added reactions yet!

--Things are actors which don't act (they just sit there)


simpleDraw: Element->DrawActor
simpleDraw e = (\_ _ -> e)

faceDraw: D.Dict Int Element -> DrawActor
faceDraw d = (\a _ -> getD (agetInt "face" a) empty d)

faceDict: [Element] -> D.Dict Int Element
faceDict li= D.empty |> foldlRot (\(arr,elt) -> D.insert arr elt) (zip [upArrow,downArrow,leftArrow,rightArrow,0] li)


addType: String -> Action -> Reaction -> DrawActor -> World2 -> World2
addType str act react da w = {w | actions <- D.insert str act w.actions,
                                  reactions <- D.insert str react w.reactions,
                                  draws <- D.insert str da w.draws}

delta = inSeconds <~ fps 10

input = sampleOn delta (Input <~ Keyboard.keysDown
                                    ~ delta)