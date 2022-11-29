{-# LANGUAGE InstanceSigs #-}
import Data.Char

isDecimalDigit :: Char -> Bool
isDecimalDigit vmi =
    case isDigit vmi of
        True    -> True
        False   -> False

isInt :: String -> Bool
isInt xs  =
    case dropWhile isDigit xs of
        ""      -> True
        _       -> False

strToInt :: String -> Int
strToInt = undefined

type Stack a = [a]
push :: Stack a -> a -> Stack a
push = undefined

