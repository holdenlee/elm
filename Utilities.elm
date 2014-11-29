module Utilities (..) where

import List (..)
import Dict as D
import Maybe as M

--Lists (Note: array is better at these things)

(!): [a] -> Int -> a
li ! i = head (drop i li)

modifyAt: Int -> [a] -> [a] -> [a]
modifyAt i li2 li = (take i li) ++ li2 ++ (drop (i+1) li)

modify:Int -> a -> [a] -> [a]
modify i x li = modifyAt i [x] li

getFirstNonzero : [Int] -> Int
getFirstNonzero li = if | isEmpty li -> 0
                        | head li /=0 -> head li
                        | otherwise -> getFirstNonzero (tail li)

listprod: [a] -> [b] -> [(a,b)]
listprod l1 l2 = concat 
                 (map (\x -> 
                           (map (\y -> (x,y)) l2))
                  l1)

--inclusive
range: Int -> Int -> [Int]
range x y = if (x>y) then [] else x::(range (x+1) y)


--(!!): [Int] -> Int -> Int
--li !! i = if (i<length li) then (li!i) else 0

--tuples 
smult:Int -> (Int,Int)-> (Int,Int)
smult c (x,y) = (c*x,c*y)

--mapping

zipMap : (a -> b -> c) -> [a] -> [b] -> [c]
zipMap f al bl = map (\(x,y) -> f x y) (zip al bl)

--folding
foldlRot: (a -> b -> b) -> [a] -> b -> b
foldlRot f x y = foldl f y x

while: (b -> Bool) -> b -> (b -> b) -> b
while f x0 update = if (f x0) 
                    then (while f (update x0) update) 
                    else x0

--getting from dicts and lists
--def is default
getD:comparable -> a -> D.Dict comparable a -> a
getD str def d = M.maybe def (\x -> x) (D.get str d)

getL: [a] -> Int -> a -> a
getL li i def = if (i<length li) then (li!i) else def

