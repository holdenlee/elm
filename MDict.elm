module MDict (..) where

import Dict as D
import Set
import Utilities (..)

type MDict comparable comparable' = D.Dict comparable (Set.Set comparable')

mget: comparable -> MDict comparable comparable' -> (Set.Set comparable')
mget x d = getD x (Set.empty) d

addM: comparable -> comparable' -> MDict comparable comparable' -> MDict comparable comparable'
addM x y d = D.insert x (Set.insert y (mget x d)) d

remM: comparable -> comparable' -> MDict comparable comparable' -> MDict comparable comparable'
remM x y d = D.insert x (Set.remove y (mget x d)) d
