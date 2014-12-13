import Color (..)
import Keyboard (..)
import Text
import Window
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Time (..)

--(!): List a -> Int -> a
--li ! i = head (drop i li)

-- input

--keysDown : Signal (List KeyCode)

type Update = Space Bool | Reset Bool | TimeDelta Float

input: Signal Update
input = mergeMany [map Space space, map Reset enter, map TimeDelta (fps 10)]

-- Model

type alias Model = {t:Float, paused:Bool, count:Int}

-- Updates

stepModel : Update -> Model -> Model
stepModel update m =  
    case update of 
      Space b -> if b then {m | paused <- not m.paused} else m
      Reset b -> if not m.paused then {m | t <- 0} else m
      TimeDelta d -> if not m.paused then {m | t <- m.t + d} else m
--warning: reset introduces a slight inaccuracy

--Display

display : (Int, Int) -> Model -> Element
display (w,h) m = 
    let n : Element
        n = txt (Text.height 50) (toString m.t)
    in container w h middle <| flow down [(txt (Text.height 50) (toString m.t)), if m.paused then (txt (Text.height 50) "Paused") else empty]
--collage w h (toForm (flow down [(txt (Text.height 50) (toString m.count)), if m.paused then (txt (Text.height 50) "Paused") else empty]))
--[toForm n |> move (0,0),
--                                           toForm (txt (Text.height 50) (toString m.count))]
--                                           (if m.paused then toForm (txt (Text.height 50) "Paused") else toForm (txt (Text.height 50) "Going"))]
--((w `div` 2) - 10, (h `div` 2) - 20)]
              
--Run

txt : (Text.Text -> Text.Text) -> String -> Element
txt f x = Text.leftAligned <| f <| Text.monospace <| (Text.color black) <| Text.fromString x

startModel : Model
startModel = {t=0, paused=False, count = 0}

gameModel = foldp stepModel startModel input

main = map2 display Window.dimensions gameModel