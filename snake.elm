import Color (..)
import Keyboard (..)
import Text
import Window
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Time (..)
import List as L
import Array as A
import Maybe as M
import Graphics.Input (..)
import Random (..)

--UTILITY
type alias Direction = (Int, Int)
type alias Loc = (Int, Int)

(+.): (Int, Int) -> Direction -> (Int, Int)
(x0,y0) +. (x1,y1) = (x0 + x1, y0 + y1) 

push : a -> A.Array a -> A.Array a
push x ar = A.append (A.fromList [x]) ar

body : A.Array a -> A.Array a
body ar = A.slice 0 -1 ar

head : A.Array a -> a
head ar = case A.get 0 ar of Just x -> x

member : a -> A.Array a -> Bool
member x ar = (A.length (A.filter (\y -> y==x) ar) >= 1)

genUntil : (a -> Bool) -> Generator a -> Seed -> (a, Seed)
genUntil f g s = 
  let
    (x, s') = generate g s
  in
    if f x 
    then (x, s')
    else genUntil f g s'

place' : Int -> Int -> (Int, Int) -> Form -> Form
place' a b (x,y) = move ((toFloat (-a))/2 + toFloat x,(toFloat -b)/2 + toFloat y)

recti : Int -> Int -> Shape
recti x y = rect (toFloat x) (toFloat y)

squari : Int -> Shape
squari x = square (toFloat x)

{-movi : (Int, Int) -> Form -> Form
movi (x,y) = move (toFloat x, toFloat y)-}

--Control
ifs: List (Bool, a) -> a -> a
ifs li z = case li of
             (b, y)::tl -> if b then y else ifs tl z
             []         -> z 

{-count : a -> A.Array a -> Int
count x ar = length (filter (==x) ar)
-}

--PARAMETERS

size = 10
xmax = 25
ymax = 25

--MODEL

--A snake is an array
type alias Snake = A.Array (Int, Int)

type alias Model = {xd : Int, yd : Int, size : Int, mouse: Loc, snake : Snake, dir : Direction, alive : Bool, seed : Seed}

init : Int -> Model
init i = 
    let
        s = initialSeed i
        (ms, s') = generate (pair (int 1 xmax) (int 1 ymax)) s
    in
        {xd = xmax, yd = ymax, size = size, mouse = ms, snake = A.fromList [(1,1)], dir = (1,0), alive = True, seed = s}

{-start : Signal Model
start = map init (map round millisecond)-}

--UPDATE
--Move the snake

step : Direction -> Model -> Model
step dir m = 
  let
    d = if dir /= (0,0)
        then dir
        else m.dir
    s = m.snake
    hd = (head s +. d)
  in
    if checkOK hd m
    then 
        if (hd == m.mouse) 
        then 
            let {-(a -> Bool) -> Generator a -> Seed -> (a, Seed)-}
                (pt, s') = genUntil (\pt -> not (pt `member` s || pt == hd)) (pair (int 1 m.xd) (int 1 m.yd)) m.seed
            in 
              {m | snake <- push hd s
                 , seed <- s'
                 , mouse <- pt
                 , dir <- d}
        else 
            
              {m | snake <- push hd (body s), dir <- d}
       else
           {m | alive <- False}

{-move: Snake -> Direction -> Snake
move s d = push (head s +. d) (body s)-}

inBounds : (Int, Int) -> Model -> Bool
inBounds (x,y) m = 
  (x > 0) && (y > 0) && (x <= m.xd) && (y <= m.yd)

checkOK: (Int,Int) -> Model -> Bool
checkOK pt m = 
  let
    s = m.snake
  in
    m.alive && (inBounds pt m) && not (pt `member` s)

--VIEW
render : Model -> Element
render m = 
  let 
      place = place' ((m.xd + 2) * m.size) ((m.yd + 2) * m.size)
      (xm, ym) = m.mouse
  in 
      collage ((m.xd + 2) * m.size) ((m.yd + 2) * m.size)
                  (L.append [recti ((m.xd + 2) * m.size) ((m.yd + 2) * m.size)
                       |> filled (rgb 0 0 0),
                   recti (m.xd * m.size) (m.yd * m.size)
                        |> filled (rgb 255 255 255),
                   squari m.size |> filled (rgb 128 0 128) |> place (xm*m.size + m.size//2,ym*m.size + m.size//2)]
                  (A.toList (A.map (\(x,y) -> squari m.size
                                   |> filled (rgb 255 0 0)
                                   |> place (x*m.size + m.size//2,y*m.size + m.size//2)) m.snake)))

--SIGNALS

inputDir : Signal Direction
inputDir = 
  map4 (\l u d r -> ifs [(l, (-1,0)), (u, (0,1)), (d, (0,-1)), (r, (1,0))] (0,0)) (isDown 37) (isDown 38) (isDown 40) (isDown 39)

input : Signal Direction
input = sampleOn (fps 20) inputDir

main = map render (foldp step (init 0) input)

--Ideas:
{-
1) Random mouse: mouse moves. 
1b) Devil mouse: mouse learns from how you move to avoid you.
2) Jumps
3) Wormholes
4) You are the mouse trying to avoid the snake
5) TRON!
-}