{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_5 where

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

{-Exercise 4-}
strModified :: Lens' (String, Bool) String
strModified = lens getStr setStr
  where
    getStr (s, _) = s
    setStr (_, _) s = (s, True)

{-Exercise 5-}
strBuild :: Lens' String String
strBuild = lens getStr setStr
  where
    getStr s = s
    setStr s1 s2 = s2 ++ s1

{-Exercise 6-}
data Builder =
  Builder
    { _context :: [String]
    , _build :: [String] -> String
    }

lastStr :: Lens' Builder String
lastStr = lens getStr setStr
  where
    getStr Builder {_context = []} = ""
    getStr Builder {_context = xs} = last xs
    setStr Builder {_context = [], _build = f} s =
      Builder {_context = [s], _build = f}
    setStr Builder {_context = xs, _build = f} s =
      Builder {_context = (init xs) ++ [s], _build = f}

newString = "Helloooo"

built = Builder {_context = ["hello", "hi"], _build = foldr (\x y -> x ++ y) ""}

built2 = set lastStr (view lastStr built) built

built3 = set lastStr newString (set lastStr newString built)

built4 = set lastStr newString built

executedBuild = _build built $ _context built

executedBuild2 = _build built2 $ _context built2

executedBuild3 = _build built3 $ _context built3

executedBuild4 = _build built4 $ _context built4

chapter3_5 :: IO ()
chapter3_5 = do
  putStrLn "Law 1"
  putStrLn $ show $ (view lastStr $ set lastStr newString built) == newString
  putStrLn "Law 2"
  putStrLn $ show $ executedBuild == executedBuild2
  putStrLn "Law 3"
  putStrLn $ show $ executedBuild4 == executedBuild3
