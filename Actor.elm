module Actor (..) where

import Dict as D
import Maybe (..)
import List
import Set

import Directions (..)
import Utilities (..)

--dictionary of information

data Info = I [Int] | S [String]

type DictInfo = D.Dict String Info

type Actor = {info: DictInfo, locs:[(Int,Int)]}

--setting and getting for DictInfo and Actor

getIs : String -> DictInfo -> [Int]
getIs str d = maybe [] (\i -> (case i of 
                               I ls -> ls
                               S ls -> [])) (D.get str d)

agetIs: String -> Actor -> [Int]
agetIs str a = getIs str a.info

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

agetSs: String -> Actor -> [String]
agetSs str a = getSs str (a.info)

getStr : String -> DictInfo -> String
getStr str d = maybe "" (\i -> (case i of 
             I ls -> ""
             S ls -> getL ls 0 "")) (D.get str d)

agetStr: String -> Actor -> String
agetStr str a = getStr str (a.info)

setStr : String -> String -> DictInfo -> DictInfo
setStr str s d = D.insert str (S [s]) d 

asetStr: String -> String -> Actor -> Actor
asetStr str s a = {a | info <- setStr str s a.info}

--basic fields
getId: Actor -> Int
getId a = getInt "id" a.info

setId: Int -> Actor -> Actor
setId i a = {a | info <- setInt "id" i a.info}

getType: Actor -> String
getType a = getStr "type" a.info

setType: String -> Actor -> Actor
setType i a = {a | info <- setStr "type" i a.info}

--getting and setting location

adirFrom: Int -> Actor -> (Int,Int)
adirFrom dir a = dirFrom dir (a.locs!0) 

setLoc: (Int,Int) -> Actor -> Actor
setLoc (x,y) a = setLocs [(x,y)] a

setLocs: [(Int,Int)] -> Actor -> Actor
setLocs li a = {a | locs <- li}

--basic actor

nullActor:Actor
nullActor = {info = D.empty, locs =[]} 
