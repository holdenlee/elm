module World (..) where

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
import Input (..)

--adict is list of all actors by id
--ilist is list of *active* actors' id in order of movement (movement is sequential)
--alocs is a map from locations to actors.
type World a = {a | l:Int, h: Int, adict: D.Dict Int Actor, ilist: A.Array Int, alocs: D.Dict (Int,Int) (Set.Set Int), text:String, curId:Int, input:Input, info:DictInfo}

getActor: Int -> World a -> Actor
getActor i w = getD i nullActor w.adict

addActor:Actor -> World a -> World a
addActor ac w = 
    let 
        newA = ac |> setId w.curId
        locs = ac.locs
    in
        { w | ilist <- A.push w.curId w.ilist, 
                       adict <- w.adict |> (D.insert w.curId newA), 
                       alocs <- w.alocs |> foldlRot (\loc -> addM loc w.curId) locs,  
                       curId <- w.curId + 1}

addActorAtLocs:Actor -> [(Int,Int)] -> World a -> World a
addActorAtLocs ac li w = w |> foldlRot (\loc wo -> wo |> addActor (ac |> setLoc loc)) li

getActorsAt: (Int,Int) -> World {}-> [Actor]
getActorsAt (x,y) w = 
    let 
        s = mget (x,y) w.alocs
    in
        List.map (\i -> getActor i w) (Set.toList s)

getFirstActorAt:(Int,Int) -> World {} -> Actor
getFirstActorAt (x,y) w = getL (getActorsAt (x,y) w) 0 nullActor

getAtSameSpace: (Actor, World {}) -> [Actor]
getAtSameSpace (a,w) = getActorsAt (a.locs!0) w

getPlayerAtSameSpace : (Actor, World {}) -> Actor
getPlayerAtSameSpace (a,w) =getL (List.filter (\x -> getType x == "player") (getAtSameSpace (a,w))) 0 nullActor

getSatisfyingAtSameSpace : (Actor -> Bool) -> (Actor, World {}) -> [Actor]
getSatisfyingAtSameSpace f (a,w) = (List.filter (\x -> f x) (getAtSameSpace (a,w)))


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

--use this ONLY if the actor doesn't change location (more efficient).
updateActor0: Actor -> Actor -> World a -> World a
updateActor0 old new w = 
    let 
        id = getId old
        remM2 = (\x -> \z -> remM x id z)
        addM2 = (\x -> \z -> addM x id z)
    in
        {w | adict <- D.insert id new w.adict}

emptyWorld:Int -> Int -> World {}
emptyWorld x y = {l=x, h=y, adict=D.empty, ilist=A.empty, alocs=D.empty, text="", curId = 0, input = nullInput, info = D.empty}

--moving in the world

inRange : World a -> (Int,Int) -> Bool
inRange w (x,y) = (y<w.h && y>=0 && x>=0 && x<w.l)

emptySpace: World a -> (Int,Int) -> Bool
emptySpace w (x,y) = (Set.empty == (mget (x,y) w.alocs))

type MoveCondition a = World a -> (Int,Int) -> Bool

defaultMoveCondition : MoveCondition a
defaultMoveCondition w sp = (inRange w sp) && (emptySpace w sp)

tryMove : World a -> (Int,Int) -> Dir -> (Int,Int)
tryMove w (x,y) dir = tryMove2 defaultMoveCondition w (x,y) dir

tryMove2: MoveCondition a -> World a -> (Int,Int) -> Dir -> (Int,Int)
tryMove2 mc w (x,y) dir = let newSpace = dirFrom dir (x,y)
                      in
                        if mc w newSpace then newSpace else (x,y)

actorInDir : Int -> (Actor, World {}) -> Actor
actorInDir i (ac,w) = (getFirstActorAt (adirFrom i ac) w)

--actorIn : Input -> Actor -> World {} -> Actor
--actorIn inp ac w = actorInDir (getOneKey inp.keys) ac w
actorIn : (Actor, World {}) -> Actor
actorIn (ac,w) = actorInDir (getOneKey w.input.keys) (ac,w)