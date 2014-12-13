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

--Utility

(!): List a -> Int -> a
li ! i = L.head (L.drop i li)

enumerate: List a -> List (Int, a)
enumerate li = L.map2 (,) [0..(L.length li - 1)] li

-- Input
(upArrow, downArrow, leftArrow, rightArrow) = (38,40,37,39)

type alias Step = List Bool

type Input = Pressed Step | TimeDelta Time

input: Signal Input
input = mergeMany [map Pressed ((\x y z w -> [x,y,z,w]) <~ isDown leftArrow
                                                        ~ isDown downArrow
                                                        ~ isDown upArrow
                                                        ~ isDown rightArrow), map TimeDelta (fps 50)]

step: Int -> Int -> Int -> Int -> Step
step w x y z = [(w==1), (x==1), (y==1), (z==1)]

--noStep : Step -> Bool
--noStep s = (s.up == False) && (s.down == False) && (s.left == False) && (s.right == False) 

none:Step 
none = step 0 0 0 0

u : Step
u = step 0 0 1 0

d : Step
d = step 0 1 0 0

l : Step
l = step 1 0 0 0

r : Step
r = step 0 0 0 1

(&) : Step -> Step -> Step
s1 & s2 = L.map2 (||) s1 s2

imp: Bool -> Bool -> Bool 
imp x y = (x==False) || (y==True)

--Return yes if the second is a subset of the first.
hitAll: Step -> Step -> Bool
hitAll s t = L.all identity (L.map2 (flip imp) s t)

type alias Dance = A.Array Step

getStep : Int -> Dance -> Step
getStep i dance = M.withDefault none (A.get i dance)

sampleDance:Dance
sampleDance = A.fromList [u, r, u, r, d]

-- Model

type alias Game = { index:Int, lastPressed: Step, pressed:Step, dance:A.Array Step, t:Time}

-- Updates

stepGame : Input -> Game -> Game
stepGame inp g = 
--if the dance is over
    if g.index >= A.length g.dance 
    then g
--if the dance is continuing
    else 
      case inp of
--only updating time
        TimeDelta d -> {g | t <- g.t + d}
        Pressed s -> 
--at all places where inp does not equal lastPressed, update the keys that are registered as being pressed
            let newPressed = L.map3 (\x y z -> if x/=y then x else z) s g.lastPressed g.pressed
            in
              if newPressed `hitAll` (getStep g.index g.dance)
--move on to next
              then {g | index <- g.index + 1, lastPressed <- s, pressed <- none}
              else {g | lastPressed <- s, pressed <- newPressed}

--Display

stepNames = ["left", "down", "up", "right"]

arrowPics1 : List String
arrowPics1 = L.map (\x -> "ddr/"++x++"1.png") stepNames

arrowPics2 : List String
arrowPics2 = L.map (\x -> "ddr/"++x++"2.png") stepNames

displayStep : (Int, Step) -> Element
displayStep (i,s) = 
    if i==0
    then flow right (L.map (\(i,x) -> if x then image 64 64 <| arrowPics2!i else spacer 64 64) (enumerate s))
    else flow right (L.map (\(i,x) -> if x then image 64 64 <| arrowPics1!i else spacer 64 64) (enumerate s))

display : (Int, Int) -> Game -> Element
display (w,h) g =
    let 
        numFit = h // 64
        danceDisplay = 
          container 256 h midTop <| 
                    flow down 
                             (L.map displayStep (enumerate (L.map (flip getStep g.dance) [(g.index)..(g.index + numFit - 1)])))
        score = txt (Text.height 50) (toString g.index)
        time = txt (Text.height 50) (toString (round (g.t/1000)))
        arrows = txt (Text.height 50) (toString g.pressed)
        rightDisplay = flow down [score, time, arrows]
    in container w h topLeft (flow right [danceDisplay, rightDisplay])

txt : (Text.Text -> Text.Text) -> String -> Element
txt f x = Text.leftAligned <| f <| Text.monospace <| (Text.color black) <| (Text.fromString x)

gameStart : Game
gameStart = {index = 0, lastPressed = none, pressed = none,  dance = sampleDance, t=0}

gameState = foldp stepGame gameStart input

main = map2 display Window.dimensions gameState