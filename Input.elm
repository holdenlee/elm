module Input (..) where

import Keyboard
import Text
import Window
import Dict as D
import Maybe (..)
import List
import Set
--user-defined modules
import Utilities (..) 

type Input = {keys:[Keyboard.KeyCode], delta:Time} 

getOneKey:[Keyboard.KeyCode] -> Keyboard.KeyCode
getOneKey li = if (isEmpty li) then 0 else (li!0)

