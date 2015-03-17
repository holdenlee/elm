{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
#-}
{-usage: SMToElm input.sm output.txt-}

module Main where
import System.Environment
import System.Directory
import Control.Monad
--import Utilities
import System.IO  
import Data.List

to01 :: String -> String
to01 = map (\x -> if x=='0' then '0' else '1')

stepToElm :: String -> String
stepToElm s = 
  if ((length s < 4) || (s == "0000"))
    then "" 
    else '[':((intersperse ',' (to01 s))++"]")

fileToElm :: String -> String
fileToElm s = 
  let
    ss = lines s
  in  
    '[':((intercalate "," $ filter (/="") (map stepToElm ss))++"]")

ioFile:: String -> String -> (String -> String) -> IO ()
ioFile inputF outputF f =
 do  
  handle <- openFile inputF ReadMode
  contents <- hGetContents handle
  writeFile outputF (f contents)

main::IO ()
main = do
  args <- getArgs
  let a0 = if (length args == 0) then "in.txt" else (args!!0)
  let a1 = if (length args <= 1) then "out.txt" else (args!!1)
  ioFile a0 a1 fileToElm
