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

type Action = (Actor, World {}) -> (Actor, World {})

getFromArg : (Actor -> Action) -> Action
getFromArg f = (\(ac,w) -> (f ac) (ac,w))

--dirFromInp : (Dir -> Action) -> Action
--dirFromInp f = (\ac w -> (f (getOneKey w.input.keys)) inp ac w)

--facing : Actor -> Int
--facing act = getFirstNonzero [get2 "facing" act.info, get2 "last_move" act.info]

nullAction:Action
nullAction t = t 

--the first actor reacts to the second actor
type Reaction = Actor -> Actor -> World {} -> World {}

nullReaction:Reaction
nullReaction _ _ w = w

--creating simple actions
simpleAction : ((Actor, World {}) -> Actor) -> Action
simpleAction f = (\(ac,w) -> 
                               let 
                                   newA = f (ac,w)
                               in      
                                   (newA, updateActor ac newA w))

simpleAction0 : ((Actor, World {}) -> Actor) -> Action
simpleAction0 f = (\(ac,w) -> 
                               let 
                                   newA = f (ac,w)
                               in      
                                   (newA, updateActor0 ac newA w))

setAction : (Actor -> Actor) -> Action
setAction f = simpleAction (\(a,w) -> f a)

dirFromInp : (Dir -> Action) -> Action
dirFromInp f = (check (\(_,w) -> Set.member (getOneKey w.input.keys) dirs)) .& (\(a,w) -> f (getOneKey w.input.keys) (a,w))

--display a message

messageAction : (Actor -> String) -> Action
messageAction f = seqActions [(\(a,w) -> (a,{w | text <- w.text ++ "\n" ++ (f a)})), succeed]

messageAction2 : ((Actor, World {}) -> String) -> Action
messageAction2 f = seqActions [(\(a,w) -> (a,{w | text <- w.text ++ "\n" ++ (f (a,w))})), succeed]

--checking actions

check : ((Actor, World {}) -> Bool) -> Action
check f = simpleAction0 (\(a,w) -> if f (a,w) then asetInt "success" 1 a else asetInt "success" 0 a)

checkActor : (Actor -> Bool) -> Action
checkActor f = check (\(a,_) -> f a)

--movement actions
moveInDir : Int -> Action
moveInDir dir = simpleAction (\(a,w) ->  
                                                  let 
                                                      oldLoc = a.locs!0
                                                  in a |> (setLoc (tryMove w oldLoc (dir))) |> (\x -> if (x.locs!0)==oldLoc 
                                                                                                      then (asetInt "success" 0 x)
                                                                                                      else (asetInt "success" 1 x)))

moveInDir2 : MoveCondition {} -> Int -> Action
moveInDir2 mc dir = simpleAction (\(a,w) ->  
                                                  let 
                                                      oldLoc = a.locs!0
                                                  in a |> (setLoc (tryMove2 mc w oldLoc (dir))) |> (\x -> if (x.locs!0)==oldLoc 
                                                                                                      then (asetInt "success" 0 x)
                                                                                                      else (asetInt "success" 1 x)))

moveIn: Action
moveIn = dirFromInp moveInDir

moveIn2: MoveCondition {} -> Action
moveIn2 mc =  dirFromInp (moveInDir2 mc)

--tryJump : World a -> Actor  -> (Int,Int) -> Actor
--tryJump w ac (x,y) = 
--    if ((inRange w (x,y)) && emptySpace w (x,y)) then setLoc (x,y) ac else ac

--tryMoves : World a -> (Int, Int) -> [Int] -> ((Int, Int), Int)
--tryMoves w (x,y) dirs = 
--    let (iend, newLoc) = while
--            (\(i,loc)->(i<length dirs && loc == (x,y)))
--            (0,(x,y))
--            (\(i,(x,y)) ->(i+1,tryMove w (x,y) (dirs!i)))
--    in (newLoc, if ((x,y)==newLoc) then 0 else dirs!(iend-1)) 

faceDir : Dir -> Action
faceDir dir = setAction (asetInt "face" dir)

face : Action
face = dirFromInp faceDir
--(check (\inp _ _ -> not (getOneKey inp.keys == 0))) .& (\inp -> (faceDir (getOneKey inp.keys)) inp) 
--.& (messageAction (\_ -> "faced"))

velocityDir: Int -> Action
velocityDir dir = setAction (asetInt "v" dir)

velocity : Action
velocity = dirFromInp velocityDir

stop: Action
stop = velocityDir 0

--kill: Actor -> Action
--kill a = (\_ _ w -> 

inertia:Action
inertia = (\(a,w) -> (moveInDir (agetInt "v" a)) (a,w)) .| stop
--seqActions [messageAction (\x -> show (agetInt "v" x)) ,(\inp a w -> (moveInDir (agetInt "v" a)) inp a w) .| stop]

inertia2: MoveCondition {} -> Action
inertia2 mc = (\(a,w) -> (moveInDir2 mc (agetInt "v" a)) (a,w)) .| stop

allActorsSatisfy : (Actor -> Bool) -> MoveCondition {}
allActorsSatisfy f = (\w loc -> inRange w loc && ([] == List.filter (\x -> not (f x)) (getActorsAt loc w)))

--combinators

seqActions: [Action] -> Action
seqActions acts = foldl (<<) nullAction acts

--caseAction: ((Actor, World) -> Bool) -> Action

--Combinators
(.&):Action -> Action -> Action
a1 .& a2 = seqActions [a1,(\(a,w) -> if ((agetInt "success" a) == 0) then (a,w) else a2 (a,w))]

(.|):Action -> Action -> Action
a1 .| a2 = seqActions [a1,(\(a,w) -> if ((agetInt "success" a) == 0) then a2 (a,w) else (a,w))]

--returns a success no matter what happens
try:Action -> Action
try a = seqActions [a, succeed]

repeat: Int -> Action -> Action
repeat n a = try (if n==0 then a else (\(a1,w) -> 
                      if ((agetInt "success" a1) == 0) then (a1,w) else (repeat (n-1) a) (a1,w)))
--the following is easier to write but less efficient?
--repeat n a = foldl (.&) (repeat n a)

setSuccess: Int -> Action
setSuccess i = simpleAction (\(a,w) -> asetInt "success" i a)

fail: Action
fail = setSuccess 0

succeed: Action
succeed = setSuccess 1

many0: Action -> Action
many0 a = try (\(a1,w) -> if ((agetInt "success" a1) == 0) then (a1,w) else (many0 a) (a (a1,w)))

many: Action -> Action
many a = (a .| fail) .& (many0 a)

getType2: Actor -> String
getType2 a = getStr "type" a.info

--getType is misbehaving.
--getType2: Actor -> String
--getType2 = getType

--make the 2nd actor do something
make: Actor -> Action -> Action
make ac action = (\(orig,w) -> 
                let 
                    t : String
                    t = getType2 ac
                in
                  if (t == "") then (orig,w) else (orig, snd (action (ac,w))))
--(added a check to see if it's null)

makeRel : ((Actor, World {}) -> Actor) -> Action -> Action
makeRel f action = (\(orig,w) -> 
                let 
                    t : String
                    t = getType2 (f (orig,w))
                in
                  if (t == "") then (orig,w) else (orig, snd (action (f (orig, w),w))))

doForAll:(b -> Action) -> ((Actor, World {}) -> [b]) -> Action
doForAll f g = (\(a,w) -> (seqActions (List.map f (g (a,w)))) (a,w))

--(\(orig,w) -> if (getType2 ((f (orig,w)) == "")) then (orig,w) else action (orig,w))

makeActorIn : Action -> Action
makeActorIn f = makeRel actorIn f

die: Action
die = simpleAction (\_ -> nullActor)

doOrFail: Action -> Action
doOrFail ac = (\(a,w) -> 
              let (a2,w2) = ac (a,w)
              in 
                if ((agetInt "success" a2) == 1)
                   then (a2,w2)
                   else fail (a,w))