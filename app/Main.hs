{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Chapter1
import Chapter2
import Chapter3_1
import Chapter3_2
import Chapter3_3
import Chapter3_4
import Chapter3_5
import Chapter3_6
import Chapter3_7
import Chapter3_8
import Chapter3_9
import Chapter4
import Chapter5_1
import Chapter5_2
import Chapter5_3
import Chapter6_1
import Chapter6_2
import Chapter6_3
import Chapter6_4
import Chapter6_5
import Chapter6_6
import Control.Applicative
import Control.Lens
import Control.Lens.Unsound
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import System.Environment

{-Linebreak Formatting Choices-}
hmed :: IO ()
hmed = do
  putStrLn "------------------------------------------------------------------"

hlow :: IO ()
hlow = do
  putStrLn "__________________________________________________________________"

hhigh :: IO ()
hhigh = do
  putStrLn "‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾"

{-Runs the 'main' IO from another module with propert formatting-}
moduleRunner :: String -> IO () -> IO () -> IO ()
moduleRunner s hor1 body = do
  hor1
  putStrLn s
  hor1
  body

{-Standardizing format-}
standardRunner :: String -> IO () -> IO ()
standardRunner s body = moduleRunner s hlow body

{-Title Generation For Sections from Chapter Numbers-}
chap :: Int -> String
chap c = "Chapter " ++ show c

abChap :: Int -> Int -> String
abChap c s = chap c ++ " Part " ++ show s

{-Pulls in the provided arguments and runs th chapter function -}
{-specified by the program argument-}
dispatch :: String -> IO ()
{-Whole Chapter-}
dispatch "chapter1" = standardRunner (chap 1) chapter1
dispatch "chapter2" = standardRunner (chap 2) chapter2
dispatch "chapter4" = standardRunner (chap 4) chapter4
{-Partial Chapters-}
dispatch "chapter3p1" = standardRunner (abChap 3 1) chapter3_1
dispatch "chapter3p2" = standardRunner (abChap 3 2) chapter3_2
dispatch "chapter3p3" = standardRunner (abChap 3 3) chapter3_3
dispatch "chapter3p4" = standardRunner (abChap 3 4) chapter3_4
dispatch "chapter3p5" = standardRunner (abChap 3 5) chapter3_5
dispatch "chapter3p6" = standardRunner (abChap 3 6) chapter3_6
dispatch "chapter3p7" = standardRunner (abChap 3 7) chapter3_7
dispatch "chapter3p8" = standardRunner (abChap 3 8) chapter3_8
dispatch "chapter3p9" = standardRunner (abChap 3 9) chapter3_9
dispatch "chapter5p1" = standardRunner (abChap 5 1) chapter5_1
dispatch "chapter5p2" = standardRunner (abChap 5 2) chapter5_2
dispatch "chapter5p3" = standardRunner (abChap 5 3) chapter5_3
dispatch "chapter6p1" = standardRunner (abChap 6 1) chapter6_1
dispatch "chapter6p2" = standardRunner (abChap 6 2) chapter6_2
dispatch "chapter6p3" = standardRunner (abChap 6 3) chapter6_3
dispatch "chapter6p4" = standardRunner (abChap 6 4) chapter6_4
dispatch "chapter6p5" = standardRunner (abChap 6 5) chapter6_5
dispatch "chapter6p6" = standardRunner (abChap 6 6) chapter6_6
{-Catch for total function -}
dispatch s = putStrLn $ s ++ ": Incorrect argument"

{-ALlows multiple arguments to be passed to the input function for multiple chapters-}
runDispatch :: [String] -> IO ()
runDispatch [] = return ()
runDispatch (x:xs) = do
  dispatch x
  runDispatch xs

{-Gets the command line args then runs the dispatcher over all of them-}
main :: IO ()
main = do
  args <- getArgs
  runDispatch args
  return ()
