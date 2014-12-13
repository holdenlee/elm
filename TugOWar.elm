import Color (..)
import Keyboard (..)
import Text
import Window
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Time (..)
import List as L

--(!): List a -> Int -> a
--li ! i = head (drop i li)

-- input

--keysDown : Signal (List KeyCode)

type alias Dir = Int

type Update = LR Dir | Enter Bool | TimeDelta Time

pressed: KeyCode -> Signal Bool
pressed x = keepIf (\_ -> True) False (isDown x)

enumerate: List a -> List (Int, a)
enumerate li = L.map2 (,) [1..(L.length li)] li

--getSignalIndex : List (Signal a) -> Signal Int
--getSignalIndex li = map (\(i,s) -> i) (mergeMany <| L.map (\(x, s) -> map (\t -> (x,t)) s) (enumerate li))

getSignalPair : List (Signal a) -> Signal (Int, a)
getSignalPair li = mergeMany <| L.map (\(x, s) -> map (\t -> (x,t)) s) (enumerate li)

getPressed : Signal Update
getPressed = map (\(i,x) -> if x==False then LR (-1) else LR i) (getSignalPair [isDown 188, isDown 190])

input: Signal Update
input = mergeMany [getPressed, map TimeDelta (fps 50), map Enter (pressed 13)]
--map LR <| getSignalIndex [pressed 188, pressed 190], map Enter (pressed 13), map TimeDelta (fps 50)]
--[map Left (press 188), map Right (press 190), map TimeDelta (fps 50)]

-- Model

--0 for left, 1 for right
left = 0
right = 1

type alias Model = {lastPressed:Dir, score:Int, t:Time}

-- Updates

penalty = 2
duration = 10000

stepModel : Update -> Model -> Model
stepModel update m =  
  if (m.t < duration) then 
    case update of 
      LR x -> if x == -1 
              then m 
              else if m.lastPressed == x then {m | score <- m.score - penalty}
                   else {m | score <- m.score + 1, lastPressed <- x}
      TimeDelta d -> {m | t <- m.t + d}
      Enter x -> m
   else 
    case update of 
      Enter x -> if x then startModel else m
      _ -> m
--warning: reset introduces a slight inaccuracy

--Display

display : (Int, Int) -> Model -> Element
display (w,h) m = 
    container w h middle <| flow down [(txt (Text.height 50) (if (m.t < duration) then toString m.t else "Game over")), (txt (Text.height 50) (toString m.score)), if (m.t >= duration) then txt (Text.height 50) "Enter to play again" else empty]
--collage w h (toForm (flow down [(txt (Text.height 50) (toString m.count)), if m.paused then (txt (Text.height 50) "Paused") else empty]))
--[toForm n |> move (0,0),
--                                           toForm (txt (Text.height 50) (toString m.count))]
--                                           (if m.paused then toForm (txt (Text.height 50) "Paused") else toForm (txt (Text.height 50) "Going"))]
--((w `div` 2) - 10, (h `div` 2) - 20)]
              
--Run

txt : (Text.Text -> Text.Text) -> String -> Element
txt f x = Text.leftAligned <| f <| Text.monospace <| (Text.color black) <| Text.fromString x

startModel : Model
startModel = {lastPressed = -1, score = 0, t =0}

gameModel = foldp stepModel startModel input

main = map2 display Window.dimensions gameModel