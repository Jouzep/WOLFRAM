{-
-- EPITECH PROJECT, 2023
-- wolfram
-- File description:
-- Lib
-}

module Lib (
    getOpts,
    defaultConf,
    exitWithMsg,
    doWolfram
) where

import Data.Maybe (fromJust)
import Data.Char
import System.Exit
import System.IO

data Conf = Conf {
    rule :: Maybe Int,
    start :: Maybe Int,
    line :: Maybe Int,
    window :: Maybe Int,
    move :: Maybe Int,
    binary :: [Bool]
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {
    rule = Nothing,
    start = Just 0,
    line = Nothing,
    window = Just 80,
    move = Just 0,
    binary = []
}

intToByte :: Int -> Int -> [Bool]
intToByte 0 8 = []
intToByte 0 0 = replicate 8 False
intToByte 0 y = intToByte 0 (y + 1) ++ [False]
intToByte x y
    | ((x `mod` 2) == 1) = intToByte (x `div` 2) (y + 1) ++ [True]
    | otherwise = intToByte(x `div` 2) (y + 1) ++ [False]

rules :: [Bool] -> [Bool] -> [Bool]
rules (True:True:True:_) list = [list !! 0]
rules (True:True:False:_) list = [list !! 1]
rules (True:False:True:_) list = [list !! 2]
rules (True:False:False:_) list = [list !! 3]
rules (False:True:True:_) list = [list !! 4]
rules (False:True:False:_) list = [list !! 5]
rules (False:False:True:_) list = [list !! 6]
rules (False:False:False:_) list = [list !! 7]
rules _ _ = [False]

printLine :: [Bool] -> IO ()
printLine [] = return ()
printLine (True:xs) = putStr "*" >> printLine xs
printLine (False:xs) = putStr " " >> printLine xs

patterns :: [Bool] -> [Bool] -> [Bool]
patterns [] _ = []
patterns (True:True:True:xs) b = 
    (rules (True:True:True:xs) b) ++ patterns (True:True:xs) b
patterns (True:True:False:xs) b = 
    (rules (True:True:False:xs) b) ++ patterns (True:False:xs) b
patterns (True:False:True:xs) b = 
    (rules (True:False:True:xs) b) ++ patterns (False:True:xs) b
patterns (True:False:False:xs) b = 
    (rules (True:False:False:xs) b) ++ patterns (False:False:xs) b
patterns (False:True:True:xs) b = 
    (rules (False:True:True:xs) b) ++ patterns (True:True:xs) b
patterns (False:True:False:xs) b = 
    (rules (False:True:False:xs) b) ++ patterns (True:False:xs) b
patterns (False:False:True:xs) b = 
    (rules (False:False:True:xs) b) ++ patterns (False:True:xs) b
patterns (False:False:False:xs) b = 
    (rules (False:False:False:xs) b) ++ patterns (False:False:xs) b
patterns (_:xs) b = patterns xs b

removeFirstAndLast :: Int -> Int -> [Bool] -> [Bool]
removeFirstAndLast first second list = 
    drop first (take ((length list) - second) list)

checkInt :: [Char] -> Int -> Bool
checkInt (x:xs) idx = if isDigit x || (x == '-' && idx == 0)
    then checkInt xs (idx + 1)
    else False
checkInt [] _ = True

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt x
    | (checkInt x 0) == False = Nothing
    | otherwise = Just (read x)

readIntNeg :: [Char] -> Maybe Int
readIntNeg [] = Nothing
readIntNeg x
    | (checkInt x 1) == False = Nothing
    | otherwise = Just (read x)

checkRule :: Maybe Int -> Maybe Int
checkRule Nothing = Nothing
checkRule (Just x)
    | x >= 0 && x <= 255 = Just x
    | otherwise = Nothing

getOpts :: Conf -> [String] -> Maybe Conf
getOpts (Conf _ s l w m _) ("--rule":x:xs) = 
    if (readInt x == Nothing || (checkRule (readInt x)) == Nothing)
        then Nothing
    else getOpts 
    (Conf (readInt(x)) s l w m (intToByte ((fromJust (readInt x))) 0)) xs
getOpts (Conf r _ l w m b) ("--start":x:xs) = 
    if (readIntNeg x == Nothing)
        then Nothing
    else getOpts (Conf r (readInt(x)) l w m b) xs
getOpts (Conf r s _ w m b) ("--lines":x:xs) = 
    if (readIntNeg x == Nothing)
        then Nothing
    else getOpts (Conf r s (readInt(x)) w m b) xs
getOpts (Conf r s l w _ b) ("--move":x:xs) = 
    if (readInt x == Nothing)
        then Nothing
    else getOpts (Conf r s l w (readInt(x)) b) xs
getOpts (Conf r s l _ m b) ("--window":x:xs) = 
    if (readIntNeg x == Nothing)
        then Nothing
    else getOpts (Conf r s l (readInt(x)) m b) xs
getOpts (Conf Nothing _ _ _ _ _) _ = Nothing
getOpts conf [] = Just conf -- When finished
getOpts _ _ = Nothing

exitWithMsg :: String -> ExitCode -> IO ()
exitWithMsg str code = hPutStrLn stderr str >> exitWith code

addMove :: Int -> [Bool] -> [Bool]
addMove moved list
    | (moved > 0) = replicate moved False ++ list 
    | (moved < 0) = list ++ replicate (-moved) False
    | otherwise = list 

createFirstLine :: Int -> [Bool]
createFirstLine size
    | ((size `mod` 2) == 1) =
    replicate (size `div` 2) False ++ [True]
    ++ replicate (size `div` 2) False
    | otherwise =
    replicate (size `div` 2) False ++ [True]
    ++ replicate (size `div` 2 - 1) False

doWolfram :: Conf -> IO()
doWolfram (Conf r s Nothing w m b) =
    loopInfinite (Conf r s Nothing w m b) 1 (addMove (fromJust(m))
    ( [False] ++ createFirstLine (fromJust(w)) ++ [False]))
doWolfram conf =
    loop conf (line conf) 1 (addMove (fromJust(move conf))
    ( [False] ++ createFirstLine (fromJust(window conf)) ++ [False]))

loop :: Conf -> Maybe Int -> Int -> [Bool] -> IO ()
loop _ (Just 0) _ _ = return ()
loop (Conf r (Just 0) l w m b) (Just x) y list =
    printing (fromJust(m)) y list >> 
    loop (Conf r (Just 0) l w m b) (Just(x - 1)) (y + 1)
    ([False, False] ++ (patterns list b) ++ [False, False])
loop (Conf r (Just s) l w m b) x y list =
    loop (Conf r (Just(s - 1)) l w m b) (x) (y + 1)
    ([False, False] ++ (patterns list b) ++ [False, False])
loop _ _ _ _ = return ()

-- loopInfinite :: Conf -> Int -> [Bool] -> IO ()
-- loopInfinite (Conf r (Just 0) l w m b) x list =
--     printing (fromJust(m)) x list >> loopInfinite (Conf r (Just 0) l w m b) (x + 1) ([False, False] ++ (patterns list b) ++ [False, False])
-- loopInfinite (Conf r (Just s) l w m b) x list =
--     loopInfinite (Conf r (Just (s - 1)) l w m b) (x + 1) ([False, False] ++ (patterns list b) ++ [False, False])
-- loopInfinite _ _ _ = return ()

loopInfinite :: Conf -> Int -> [Bool] -> IO ()
loopInfinite (Conf r (Just 0) l w m b) y list =
    printing (fromJust(m)) y list >> 
    loopInfinite (Conf r (Just 0) l w m b) (y + 1)
    ([False, False] ++ (patterns list b) ++ [False, False])
loopInfinite (Conf r (Just s) l w m b) y list =
    loopInfinite (Conf r (Just(s - 1)) l w m b) (y + 1)
    ([False, False] ++ (patterns list b) ++ [False, False])
loopInfinite _ _ _ = return ()

printing :: Int -> Int -> [Bool] -> IO ()
printing moved y list
    | (moved < 0) = printLine (removeFirstAndLast (y + (-moved)) y list)
    >> putStrLn ""
    | (moved > 0) = 
    printLine (removeFirstAndLast y (y + moved) list) >> putStrLn ""
    | otherwise = printLine (removeFirstAndLast y y list) >> putStrLn ""