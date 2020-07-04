{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_8 where

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

data Time =
  Time
    { _hours :: Int
    , _mins :: Int
    }
  deriving (Show)

clamp :: Int -> Int -> Int -> Int
clamp minV maxV a = min maxV . max minV $ a

hours :: Lens' Time Int
hours = lens getter setter
  where
    getter (Time h _) = h
    setter (Time _ m) newHours = Time (clamp 0 23 newHours) m

mins :: Lens' Time Int
mins = lens getter setter
  where
    getter (Time _ m) = m
    setter (Time h _) newMins = Time h (clamp 0 59 newMins)

time = Time 3 10

hours' :: Lens' Time Int
hours' = lens getter setter
  where
    getter (Time h _) = h
    setter (Time _ m) newHours = Time (newHours `mod` 24) m

mins' :: Lens' Time Int
mins' = lens getter setter
  where
    getter (Time _ m) = m
    setter (Time h _) newMins =
      Time ((h + (newMins `div` 60)) `mod` 24) (newMins `mod` 60)

chapter3_8 :: IO ()
chapter3_8 = do
  putStrLn . show $ time
  putStrLn . show $ set hours 40 time
  putStrLn . show $ set mins (-10) time
  putStrLn . show $ over mins' (+ 55) time
  putStrLn . show $ over mins' (subtract 20) time
  putStrLn . show $ over mins' (+ 1) (Time 23 59)
