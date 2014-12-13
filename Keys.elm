import Color (..)
import Keyboard (..)
import Text
import Window
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)

--(!): List a -> Int -> a
--li ! i = head (drop i li)

-- input

--keysDown : Signal (List KeyCode)

-- Model

type State = Going | Paused

type alias Model = {keys : List Int}

-- Updates

stepModel : (List KeyCode) -> Model -> Model
stepModel li m =  {m | keys <- li}

--Display

display : (Int, Int) -> Model -> Element
display (w,h) m = 
    let n : Element
        n = txt (Text.height 50) (toString m.keys)
    in container w h middle <| collage w h [toForm n |> move (0,0)]
--((w `div` 2) - 10, (h `div` 2) - 20)]
              
--Run

txt : (Text.Text -> Text.Text) -> String -> Element
txt f x = Text.leftAligned <| f <| Text.monospace <| (Text.color black) <| Text.fromString x

startModel : Model
startModel = {keys = []}

gameModel = foldp stepModel startModel keysDown

main = map2 display Window.dimensions gameModel