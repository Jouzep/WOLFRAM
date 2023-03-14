{-
-- EPITECH PROJECT, 2023
-- wolfram
-- File description:
-- Lib
-}

module Main (main) where
import Lib
import System.Environment
import System.Exit

main :: IO ()
main = do
    arg <- getArgs
    let res = getOpts defaultConf arg
    case res of
        Just conf -> doWolfram conf
        Nothing ->  (exitWithMsg "Error Args" (ExitFailure 84))