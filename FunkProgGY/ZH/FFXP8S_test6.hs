import Data.Char
import Data.List
import Distribution.SPDX 

toString :: [Int] -> String
toString [] = ""
toString (x:xs) = chr x : toString xs