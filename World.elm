module World (..) where

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
import Input (..)

--adict is list of all actors by id
--alist is list of *active* actors' id in order of movement (movement is sequential)
--alocs is a map from locations to actors.
type World a = {a | l:Int, h: Int, adict: D.Dict Int Actor, alist: [Int], alocs: D.Dict (Int,Int) (Set.Set Int), text:String, curId:Int}

getActor: Int -> World a -> Actor
getActor i w = getD i nullActor w.adict

addActor:Actor -> World a -> World a
addActor ac w = 
    let 
        newA = ac |> setId w.curId
        locs = ac.locs
    in
        { w | alist <- w.alist ++ [w.curId], 
                       adict <- w.adict |> (D.insert w.curId newA), 
                       alocs <- w.alocs |> foldlRot (\loc -> addM loc w.curId) locs,  
                       curId <- w.curId + 1}

getFirstActorAt:(Int,Int) -> World {} -> Actor
getFirstActorAt (x,y) w = 
    let 
        s = mget (x,y) w.alocs
            --set
        l = List.map (\i -> getActor i w) (Set.toList s)
    in
      getL l 0 nullActor


--this is inefficient if actor is large (ex. Snake)
updateActor: Actor -> Actor -> World a -> World a
updateActor old new w = 
    let 
        id = getId old
        remM2 = (\x -> \z -> remM x id z)
        addM2 = (\x -> \z -> addM x id z)
        al2 = w.alocs |> (foldlRot remM2 old.locs) |> (foldlRot addM2 new.locs)
    in
        {w | alocs <- al2, adict <- D.insert id new w.adict}

emptyWorld:Int -> Int -> World {}
emptyWorld x y = {l=x, h=y, adict=D.empty, alist=[], alocs=D.empty, text="", curId = 0}

--moving in the world

inRange : World a -> (Int,Int) -> Bool
inRange w (x,y) = (y<w.h && y>=0 && x>=0 && x<w.l)

emptySpace: World a -> (Int,Int) -> Bool
emptySpace w (x,y) = (Set.empty == (mget (x,y) w.alocs))

tryMove : World a -> (Int,Int) -> Int -> (Int,Int)
tryMove w (x,y) dir = let newSpace = dirFrom dir (x,y)
                      in
                        if (inRange w newSpace) && (emptySpace w newSpace) then newSpace else (x,y)

actorInDir : Int -> Actor -> World {} -> Actor
actorInDir i ac w = (getFirstActorAt (adirFrom i ac) w)

actorIn : Input -> Actor -> World {} -> Actor
actorIn inp ac w = actorInDir (getOneKey inp.keys) ac w

