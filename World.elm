module World where

import Keyboard
import Text
import Window
import Dict as D
import Maybe (..)
import List
import Set
--import Utilities (..)

(!): [a] -> Int -> a
li ! i = head (drop i li)

zipMap : (a -> b -> c) -> [a] -> [b] -> [c]
zipMap f al bl = List.map (\(x,y) -> f x y) (zip al bl)


--(!!): [Int] -> Int -> Int
--li !! i = if (i<length li) then (li!i) else 0

--def is default
getD:comparable -> a -> D.Dict comparable a -> a
getD str def d = maybe def (\x -> x) (D.get str d)

getL: [a] -> Int -> a -> a
getL li i def = if (i<length li) then (li!i) else def

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

upFrom : (Int,Int) -> (Int,Int)
upFrom (x,y) = (x,y+1)

leftFrom : (Int,Int) -> (Int,Int)
leftFrom (x,y) = (x-1,y)

rightFrom : (Int,Int) -> (Int,Int)
rightFrom (x,y) = (x+1,y)

downFrom : (Int,Int) -> (Int,Int)
downFrom (x,y) = (x,y-1)

data Info = I [Int] | S [String]

type DictInfo = D.Dict String Info

getIs : String -> DictInfo -> [Int]
getIs str d = maybe [] (\i -> (case i of 
                               I ls -> ls
                               S ls -> [])) (D.get str d)

getInt : String -> DictInfo -> Int
getInt str d = maybe 0 (\i -> (case i of 
             I ls -> getL ls 0 0
             S ls -> 0)) (D.get str d)

agetInt: String -> Actor -> Int
agetInt str a = getInt str (a.info)

setInt : String -> Int -> DictInfo -> DictInfo
setInt str i d = D.insert str (I [i]) d 

asetInt: String -> Int -> Actor -> Actor
asetInt str i a = {a | info <- setInt str i a.info}

getSs : String -> DictInfo -> [String]
getSs str d = maybe [] (\i -> (case i of 
             I ls -> []
             S ls -> ls)) (D.get str d)

getStr : String -> DictInfo -> String
getStr str d = maybe "" (\i -> (case i of 
             I ls -> ""
             S ls -> getL ls 0 "")) (D.get str d)

setStr : String -> String -> DictInfo -> DictInfo
setStr str s d = D.insert str (S [s]) d 

dirFrom : Int -> (Int,Int) -> (Int,Int)
dirFrom dir (x,y) = if | dir==downArrow -> downFrom (x,y)
                       | dir==upArrow -> upFrom (x,y)
                       | dir==leftArrow -> leftFrom (x,y)
                       | dir==rightArrow -> rightFrom (x,y)
                       | otherwise -> (x,y)

getFirstNonzero : [Int] -> Int
getFirstNonzero li = if | isEmpty li -> 0
                        | head li /=0 -> head li
                        | otherwise -> getFirstNonzero (tail li)

--facing : Actor -> Int
--facing act = getFirstNonzero [get2 "facing" act.info, get2 "last_move" act.info]

modifyAt: Int -> [a] -> [a] -> [a]
modifyAt i li2 li = (take i li) ++ li2 ++ (drop (i+1) li)

modify:Int -> a -> [a] -> [a]
modify i x li = modifyAt i [x] li

--phantomActor: Thing -> Bool
--phantomActor a = D.member "phantom" a.info

type Actor = {info: DictInfo, locs:[(Int,Int)]}

nullActor:Actor
nullActor = {info = D.empty, locs =[]} 

setLoc: (Int,Int) -> Actor -> Actor
setLoc (x,y) a = setLocs [(x,y)] a

setLocs: [(Int,Int)] -> Actor -> Actor
setLocs li a = {a | locs <- li}

getId: Actor -> Int
getId a = getInt "id" a.info

setId: Int -> Actor -> Actor
setId i a = {a | info <- setInt "id" i a.info}

getType: Actor -> String
getType a = getStr "type" a.info

setType: String -> Actor -> Actor
setType i a = {a | info <- setStr "type" i a.info}

--adict is list of all actors by id
--alist is list of *active* actors' id in order of movement (movement is sequential)
--alocs is a map from locations to actors.
type World a = {a | l:Int, h: Int, adict: D.Dict Int Actor, alist: [Int], alocs: D.Dict (Int,Int) (Set.Set Int), text:String, curId:Int}

emptyWorld:Int -> Int -> World {}
emptyWorld x y = {l=x, h=y, adict=D.empty, alist=[], alocs=D.empty, text="", curId = 0}

type MDict comparable = D.Dict comparable (Set.Set Int)

mget: comparable -> MDict comparable -> (Set.Set Int)
mget x d = getD x (Set.empty) d

addM: comparable -> Int -> MDict comparable -> MDict comparable
addM x y d = D.insert x (Set.insert y (mget x d)) d

remM: comparable -> Int -> MDict comparable -> MDict comparable
remM x y d = D.insert x (Set.remove y (mget x d)) d

getActor: Int -> World a -> Actor
getActor i w = getD i nullActor w.adict

type Action = Input -> Actor -> World {} -> World {}

nullAction:Action
nullAction _ _ w = w

--the first actor reacts to the second actor
type Reaction = Input -> Actor -> Actor -> World {} -> World {}

nullReaction:Reaction
nullReaction _ _ _ w = w

type World2 = World {actions: D.Dict String Action, reactions: D.Dict String Reaction, draws: D.Dict String (Actor -> (Int,Int) -> Element)}
--todo: change the last one to Element -> Element for overlay?

emptyWorld2:Int -> Int -> World2
emptyWorld2 x y = 
    let ew = emptyWorld x y 
    in {l=x, h=y, adict=D.empty, alist=[], alocs=D.empty, text="", curId = 0, actions = D.empty, reactions = D.empty, draws = D.empty}
--{ew | actions = D.empty, reactions = D.empty, draw = D.empty}

--emptyE:Element
--emptyE=flow right []

drawActor: Actor -> (Int,Int) -> World2 -> Element
drawActor a (x,y) w = (getD (getType a) (\_ _ -> empty) w.draws) a (x,y)

foldlRot: (a -> b -> b) -> [a] -> b -> b
foldlRot f x y = foldl f y x

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
--also check if the actor died.

messageAction : (Actor -> String) -> (Input -> Actor -> World a -> World a)
messageAction f = (\_ a w -> {w | text <- w.text ++ "\n" ++ (f a)})

simpleAction : (Input -> Actor -> World a -> Actor) -> (Input -> Actor -> World a -> World a)
simpleAction f = (\inp -> (\ac -> (\w -> 
                               let 
                                   newA = f inp ac w
                               in      
                                   updateActor ac newA w)))

seqActions: [Action] -> Action
seqActions acts = 
    (\inp -> \actor -> \w -> 
             let id = getId actor 
             in
               foldl (\action -> (\w1 -> 
                                 action inp (getActor id w1) w1)) w acts) 


--careful! if actor self-destructs...

type Input = {keys:[Keyboard.KeyCode], delta:Time} 

while: (b -> Bool) -> b -> (b -> b) -> b
while f x0 update = if (f x0) 
                    then (while f (update x0) update) 
                    else x0

stepWorld : Input -> World2 -> World2
stepWorld inp w = 
    let 
        w00 = {w - actions}
        w01 = {w00 - reactions}
        w0 = {w01 - draws | text = ""}
        (_,w2) = while 
                   (\(i,wo)-> (i<(length wo.alist))) 
                   (0,w0) 
                   (\(i,wo) -> 
                       let a = getActor (wo.alist!i) wo
                       in (i+1, (maybe wo (\action -> action inp a wo) (D.get (getType a) w.actions))))
        w20 = {w2 | actions = w.actions}
        w21 = {w20 | reactions = w.reactions}
    in {w21 | draws = w.draws}
--    in {w2 | actions =  w.actions, reactions = w.reactions, draw = w.draw}
--haven't added reactions yet!

--change to allow phantom
emptySpace: World a -> (Int,Int) -> Bool
emptySpace w (x,y) = (Set.empty == (mget (x,y) w.alocs))

tryMove : World a -> (Int,Int) -> Int -> (Int,Int)
tryMove w (x,y) dir = if | dir==upArrow && y+1<w.h && emptySpace w (x,y+1) -> (x,y+1)
                         | dir==downArrow && y-1>=0 && emptySpace w (x,y-1) -> (x,y-1)
                         | dir==leftArrow && x-1>=0 && emptySpace w (x-1,y) -> (x-1,y)
                         | dir==rightArrow && x+1<w.h && emptySpace w (x+1,y) -> (x+1,y)
                         | otherwise -> (x,y)

inRange : World a -> (Int,Int) -> Bool
inRange w (x,y) = (y<w.h && y>=0 && x>=0 && x<w.h)

tryJump : World a -> Actor  -> (Int,Int) -> Actor
tryJump w ac (x,y) = 
    if ((inRange w (x,y)) && emptySpace w (x,y)) then setLoc (x,y) ac else ac

tryMoves : World a -> (Int, Int) -> [Int] -> ((Int, Int), Int)
tryMoves w (x,y) dirs = 
    let (iend, newLoc) = while
            (\(i,loc)->(i<length dirs && loc == (x,y)))
            (0,(x,y))
            (\(i,(x,y)) ->(i+1,tryMove w (x,y) (dirs!i)))
    in (newLoc, if ((x,y)==newLoc) then 0 else dirs!(iend-1)) 

--

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

--Things are actors which don't act (they just sit there)

type DrawActor = (Actor -> (Int,Int) -> Element)
--D.Dict String (Actor -> (Int,Int) -> Element)

simpleDraw: Element->DrawActor
simpleDraw e = (\_ _ -> e)

addType: String -> Action -> Reaction -> DrawActor -> World2 -> World2
addType str act react da w = {w | actions <- D.insert str act w.actions,
                                  reactions <- D.insert str react w.reactions,
                                  draws <- D.insert str da w.draws}

getOneKey:[Keyboard.KeyCode] -> Keyboard.KeyCode
getOneKey li = if (isEmpty li) then 0 else (li!0)

player:Actor
player = nullActor |> setType "player" |> setLoc (0,0)

playerAction:Action
playerAction = seqActions [simpleAction (\inp -> (\a -> (\w -> setLoc (tryMove w (a.locs!0) (getOneKey inp.keys)) a))), messageAction (\a -> show (a.locs!0))]

playerDraw:DrawActor
playerDraw = simpleDraw (image 32 32 "/player.jpg")

blockDude:Actor
blockDude = nullActor |> setType "player" |> setLoc (0,0) |> (\x -> {x | info <- setInt "carrying" 0 x.info})

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

worldStart:Int -> Int -> World2
worldStart x y = (emptyWorld2 x y) |> (addActor player) |> addType "player" playerAction nullReaction playerDraw

--the window to display
viewCoord:World2 -> (Int,Int,Int,Int)
viewCoord w = (0,0,14,14)

--given the width w, height h, (x,y) where the elements should be situated, the elements, creates an element combining all of these.
collageAbs : Int -> Int -> [(Int,Int)] -> [Element] -> Element
collageAbs w h tuples elems = 
    collage w h (zipMap (\(x,y) -> \elem -> move ( (-(toFloat w)/2 + (toFloat (widthOf elem))/2 + toFloat x),  (-(toFloat h)/2 + (toFloat (heightOf elem))/2 + toFloat y)) (toForm elem)) tuples elems)

dot:Int -> (Int,Int)-> (Int,Int)
dot c (x,y) = (c*x,c*y)


listprod: [a] -> [b] -> [(a,b)]
listprod l1 l2 = concat 
                 (List.map (\x -> 
                           (List.map (\y -> (x,y)) l2))
                  l1)

--inclusive
range: Int -> Int -> [Int]
range x y = if (x>y) then [] else x::(range (x+1) y)

display : World2 -> Element
display w =
   let 
       (x0,y0,x1,y1) = viewCoord w
       coordList = listprod (range x0 x1) (range y0 y1)
       -- a list of (x,y),actor@(x,y)
       drawList:[((Int,Int),Int)]
       drawList = concat (List.map (\(x,y) -> List.map (\a -> ((x,y), a)) (Set.toList (mget (x,y) w.alocs))) coordList)
       -- the coordinates to draw
       drawCoords = List.map (\(x,y) -> pixels `dot` x) drawList
       drawPics = List.map (\(t,a) -> drawActor (getActor a w) t w) drawList
   in
       collageAbs (wp+sidebarL) hp ((0,0)::(480,450)::drawCoords)
            ((image 480 480 "field.jpg")::(plainText w.text)::drawPics)


delta = inSeconds <~ fps 10

input = sampleOn delta (Input <~ Keyboard.keysDown
                                    ~ delta)

worldState = foldp stepWorld (worldStart wd ht) input

main = lift display worldState

--Combinators
(.&):Action -> Action -> Action
a1 .& a2 = seqActions [a1,(\inp a w -> if ((agetInt "success" a) == 0) then w else a2 inp a w)]

(.|):Action -> Action -> Action
a1 .| a2 = seqActions [a1,(\inp a w -> if ((agetInt "success" a) == 0) then a2 inp a w else w)]

--dudeAction:Action
--dudeAction inp a w = 
--    let
--        key = getOneKey inp
--    in
--      if (key==leftArrow || key==rightArrow)
--         then 