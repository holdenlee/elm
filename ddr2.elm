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
import Dances (..)
import Graphics.Input (..)


-- TOP LEVEL

-- Model

type State = Setup | Ingame Game | Results Scores

type alias Scores = Int {- update later -}

type alias Model = {state : State, settings : Settings}

type alias Settings = {dance : Dance, rpt : Bool}

startModel : Model
startModel = {state = Setup, settings = default}

default : Settings
default = {dance = max300, rpt = False}

-- Display

displayTL : (Int, Int) -> Model -> Element
displayTL (w,h) m = case m.state of
                      Ingame g -> display (w,h) g
                      Setup -> viewSettings

-- Update (menu)

type Update 
    = UpdateRpt Bool 
    | UpdateSong Dance 
    | Start 
    | UpdateInput Input
    | Esc

usong : Dance -> Settings -> Settings
usong d s = {s | dance <- d}

urpt : Bool -> Settings -> Settings
urpt b s = {s | rpt <- b}

updateSettings : Update -> Settings -> Settings
updateSettings u s = case u of
               UpdateSong d -> usong d s
               UpdateRpt b -> urpt b s
               _ -> s

update : Update -> Model -> Model
update u m = case m.state of
               Setup -> case u of 
                          Start -> {m | state <- Ingame (gameStart m.settings.dance m.settings.rpt)}
                          _ -> {m | settings <- updateSettings u m.settings}
               Ingame g -> case u of 
                          UpdateInput inp -> {m | state <- Ingame (stepGame inp g)}
                          Esc -> {m | state <- Setup}

updateChannel : Channel Update
updateChannel = channel (UpdateRpt False)

songList : List (String, Dance)
songList = [("MAX 300", max300), ("MAX 300 End", maxEndPart)]

rptList : List (String, Bool)
rptList = [("Normal", False), ("Nonstop", True)]

viewSettings : Element
viewSettings = flow down 
                        [flow right [Text.leftAligned <| Text.fromString "Song", dropDown (\x -> send updateChannel (UpdateSong x)) songList],
                         flow right [Text.leftAligned <| Text.fromString "Mode", dropDown (\x -> send updateChannel (UpdateRpt x)) rptList],
                         button (send updateChannel (Start)) "Start game"]

-- Input
(upArrow, downArrow, leftArrow, rightArrow) = (38,40,37,39)

type Input = Pressed Step | TimeDelta Time

input: Signal Input
input = mergeMany [map Pressed ((\x y z w -> [x,y,z,w]) <~ isDown leftArrow
                                                        ~ isDown downArrow
                                                        ~ isDown upArrow
                                                        ~ isDown rightArrow), map TimeDelta (fps 50)]

esc : Signal Update
esc = map (\x -> Esc) (isDown 27)

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



getStep : Int -> Dance -> Step
getStep i dance = M.withDefault none (A.get i dance)

sampleDance:Dance
sampleDance = A.fromList [u, r, u, r, d, l, d, l]

-- Model

type alias Game = { index:Int, lastPressed: Step, pressed:Step, dance:A.Array Step, t:Time, rpt:Bool}

-- Updates

stepGame : Input -> Game -> Game
stepGame inp g = 
--if the dance is over
    if g.index >= A.length g.dance 
    then 
      if g.rpt 
--restart if in "nonstop" mode
      then (gameStart g.dance g.rpt) 
      else g
--if the dance is continuing
    else 
      case inp of
--only updating time
        TimeDelta d -> {g | t <- g.t + d}
        Pressed s -> 
--{g | pressed <- s}
--at all places where inp does not equal lastPressed, update the keys that are registered as being pressed
--
            let newPressed = L.map3 (\x y z -> if x/=y then x else z) s g.lastPressed g.pressed
            in
              if newPressed `hitAll` (getStep g.index g.dance)
--move on to next
              then {g | index <- g.index + 1, lastPressed <- s, pressed <- none}
              else {g | lastPressed <- s, pressed <- newPressed}
--
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
        score = txt (Text.height 50) ("Score: " ++ toString g.index)
        time = txt (Text.height 50) ("Time : " ++ toString (round (g.t/1000)))
        arrows = txt (Text.height 50) (toString g.pressed)
        rightDisplay = flow down [score, time, arrows]
    in container w h topLeft (flow right [danceDisplay, rightDisplay])

txt : (Text.Text -> Text.Text) -> String -> Element
txt f x = Text.leftAligned <| f <| Text.monospace <| (Text.color black) <| (Text.fromString x)

gameStart : Dance -> Bool -> Game
gameStart d b = {index = 0, lastPressed = none, pressed = none,  dance = d, t=0, rpt = b}

{-gameState = foldp stepGame (gameStart d b) input-}

--type Status = InGame Game | Waiting
{-model : Signal Model
model =
  Signal.subscribe updateChan
    |> Signal.foldp update emptyModel


updateChan : Signal.Channel Update
updateChan =
  Signal.channel Submit-}

model : Signal Model
model = (mergeMany [esc, (map UpdateInput input), (subscribe updateChannel)]) |> foldp update startModel

main = map2 displayTL Window.dimensions model

