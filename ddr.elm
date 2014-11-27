import Keyboard
import Text
import Window

(!): [a] -> Int -> a
li ! i = head (drop i li)

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

type Step = {up:Bool, down:Bool, left:Bool, right:Bool}
--type Dir = { x:Float, y:Float }
type Input = {up:Bool, down:Bool, left:Bool, right:Bool}
--doesn't like type aliases...
--https://groups.google.com/forum/#!msg/elm-discuss/anCtDiIHY3g/xzoKr76T-g4J

toList: Step -> [Bool]
toList s = [s.up, s.down, s.left, s.right]

step: Int -> Int -> Int -> Int -> Step
step w x y z = {up = (w==1), down = (x==1), left = (y==1), right = (z==1)}

noStep : Step -> Bool
noStep s = (s.up == False) && (s.down == False) && (s.left == False) && (s.right == False) 

--up:Dir
--up = {x=0,y=1}
--down = {x=0,y=-1}
--left = {x=-1,y=0}
--right = {x=1,y=1}
none:Step 
none = step 0 0 0 0

u : Step
u = step 1 0 0 0

d : Step
d = step 0 1 0 0

l : Step
l = step 0 0 1 0

r : Step
r = step 0 0 0 1

(&) : Step -> Step -> Step
s1 & s2 = {up = s1.up || s2.up, down = s1.down || s2.down, left = s1.left || s2.left, right = s2.right || s2.right}

imp: Bool -> Bool -> Bool 
imp x y = (x==False) || (y==True)

--does the first step match the second? Return yes if the first is a subset of the second.
matches: Step -> Step -> Bool
matches s t = foldl1 (&&) (zipWith (flip imp) (toList s) (toList t))

type Dance = [Step]

sampleDance:Dance
sampleDance = [u, r, u, r, d]

-- input

input : Signal Input
input = Input <~ Keyboard.isDown upArrow
        ~ Keyboard.isDown downArrow
        ~ Keyboard.isDown leftArrow
        ~ Keyboard.isDown rightArrow

-- Model

type Game = { index:Int, lastPress:Step, dance:[Step] }

-- Updates

stepGame : Input -> Game -> Game
stepGame ({up,down,left,right} as inp) ({index, lastPress, dance} as g) =
--if no arrows pressed. This is actually subsumed by the last
--    if | noStep inp -> {g | lastPress <- none}
--if the dance is over
    if | index > length dance -> g
--if the arrow matches, and it wasn't just pressed, then return (index + 1, this press, dance)
       | (lastPress /= dance ! index) && (matches inp (dance ! index)) -> {g | index <- index + 1, lastPress <- inp}
--if the arrow doesn't match
       | otherwise -> {g | lastPress <- inp}

--Display

display : (Int, Int) -> Game -> Element
display (w,h) ({index, lastPress, dance} as g) = 
    let score : Element
        score = txt (Text.height 50) (show g.index)
    in container w h middle <| collage w h [toForm score |> move (0,0)]
--((w `div` 2) - 10, (h `div` 2) - 20)]
              
--Run

-- http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Signal#foldp

txt : (Text -> Text) -> String -> Element
txt f x = leftAligned <| f <| monospace <| (Text.color black) <| (toText x)

gameStart : Game
gameStart = {index = 0, lastPress = none, dance = sampleDance}

gameState = foldp stepGame gameStart input

main = lift2 display Window.dimensions gameState