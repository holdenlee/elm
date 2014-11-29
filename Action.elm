module Action (..) where

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
import Input (..)

type Action = Input -> Actor -> World {} -> World {}

getFromArg : (Actor -> Action) -> Action
getFromArg f = (\inp ac w -> (f ac) inp ac w)

dirFromInp : (Int -> Action) -> Action
dirFromInp f = (\inp ac w -> (f (getOneKey inp.keys)) inp ac w)

--facing : Actor -> Int
--facing act = getFirstNonzero [get2 "facing" act.info, get2 "last_move" act.info]

nullAction:Action
nullAction _ _ w = w

--the first actor reacts to the second actor
type Reaction = Input -> Actor -> Actor -> World {} -> World {}

nullReaction:Reaction
nullReaction _ _ _ w = w

--creating simple actions
simpleAction : (Input -> Actor -> World a -> Actor) -> (Input -> Actor -> World a -> World a)
simpleAction f = (\inp -> (\ac -> (\w -> 
                               let 
                                   newA = f inp ac w
                               in      
                                   updateActor ac newA w)))

setAction : (Actor -> Actor) -> Action
setAction f = simpleAction (\_ a _ -> f a)

readFromDirInput : (Int -> Action) -> Action
readFromDirInput f = (check (\inp _ _ -> Set.member (getOneKey inp.keys) dirs)) .& (\inp -> (f (getOneKey inp.keys)) inp) 

--display a message

messageAction : (Actor -> String) -> (Input -> Actor -> World a -> World a)
messageAction f = (\_ a w -> {w | text <- w.text ++ "\n" ++ (f a)})

messageAction2 : (Input -> Actor -> World a -> String) -> (Input -> Actor -> World a -> World a)
messageAction2 f = (\inp a w -> {w | text <- w.text ++ "\n" ++ (f inp a w)})

--checking actions

check : (Input -> Actor -> World {} -> Bool) -> Action
check f = simpleAction (\inp a w -> if f inp a w then asetInt "success" 1 a else asetInt "success" 0 a)

checkActor : (Actor -> Bool) -> Action
checkActor f = check (\_ a _ -> f a)

--movement actions
moveInDir : Int -> Action
moveInDir dir = simpleAction (\inp -> (\a -> (\w -> 
                                                  let 
                                                      oldLoc = a.locs!0
                                                  in a |> (setLoc (tryMove w oldLoc (dir))) |> (\x -> if (x.locs!0)==oldLoc 
                                                                                                      then (asetInt "success" 0 x)
                                                                                                      else (asetInt "success" 1 x)))))

moveIn: Action
moveIn = (\inp -> (moveInDir (getOneKey inp.keys)) inp)

--tryJump : World a -> Actor  -> (Int,Int) -> Actor
--tryJump w ac (x,y) = 
--    if ((inRange w (x,y)) && emptySpace w (x,y)) then setLoc (x,y) ac else ac

tryMoves : World a -> (Int, Int) -> [Int] -> ((Int, Int), Int)
tryMoves w (x,y) dirs = 
    let (iend, newLoc) = while
            (\(i,loc)->(i<length dirs && loc == (x,y)))
            (0,(x,y))
            (\(i,(x,y)) ->(i+1,tryMove w (x,y) (dirs!i)))
    in (newLoc, if ((x,y)==newLoc) then 0 else dirs!(iend-1)) 

faceDir : Int -> Action
faceDir dir = setAction (asetInt "face" dir)

face : Action
face = readFromDirInput faceDir
--(check (\inp _ _ -> not (getOneKey inp.keys == 0))) .& (\inp -> (faceDir (getOneKey inp.keys)) inp) 
--.& (messageAction (\_ -> "faced"))

velocityDir: Int -> Action
velocityDir dir = setAction (asetInt "v" dir)

velocity : Action
velocity = readFromDirInput velocityDir

stop: Action
stop = velocityDir 0

--kill: Actor -> Action
--kill a = (\_ _ w -> 

inertia:Action
inertia = seqActions [messageAction (\x -> show (agetInt "v" x)) ,(\inp a w -> (moveInDir (agetInt "v" a)) inp a w) .| stop]

--combinators

seqActions: [Action] -> Action
seqActions acts = 
    (\inp -> \actor -> \w -> 
             let id = getId actor 
             in
               foldl (\action -> (\w1 -> 
                                 action inp (getActor id w1) w1)) w acts) 

--Combinators
(.&):Action -> Action -> Action
a1 .& a2 = seqActions [a1,(\inp a w -> if ((agetInt "success" a) == 0) then w else a2 inp a w)]

(.|):Action -> Action -> Action
a1 .| a2 = seqActions [a1,(\inp a w -> if ((agetInt "success" a) == 0) then a2 inp a w else w)]

try:Action -> Action
try a = seqActions [a, setAction (\a -> asetInt "success" 1 a)]

repeat: Int -> Action -> Action
repeat n a = (if n==0 then a else (\inp a1 w -> if ((agetInt "success" a1) == 0) then w else (repeat (n-1) a) inp (getActor (getId a1) w) w)) .& (setSuccess 1)
--the following is easier to write but less efficient?
--repeat n a = foldl (.&) (repeat n a)

setSuccess: Int -> Action
setSuccess i = simpleAction (\inp a w -> asetInt "success" i a)

fail: Action
fail = setSuccess 0

many0: Action -> Action
many0 a = (\inp a1 w -> if ((agetInt "success" a1) == 0) then w else (many0 a) inp (getActor (getId a1) w) w)

many: Action -> Action
many a = (a .| fail) .& (many0 a)

--make the 2nd actor do something
make: Actor -> Action -> Action
make a action = (\inp _ w -> if getType a == "" then w else action inp a w)
--(added a check to see if it's null)

makeActorIn : Action -> Action
makeActorIn f = (\inp a w -> (make (actorIn inp a w) f) inp a w)
