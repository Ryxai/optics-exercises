{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter1 where

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

newValue = "Firefly"

value1 = "Coffee"

value2 = "Red Bull"

structure = ("Simpsons", "Seinfeld")

chapter1 :: IO ()
chapter1 = do
  putStrLn $ "Law 1"
  putStrLn $ show $ view _1 (set _1 newValue ("Star Wars", "Star Trek"))
  putStrLn $
    show $ (view _1 (set _1 newValue ("Star Wars", "Star Trek")) == newValue)
  putStrLn "Law 2"
  putStrLn $ show $ set _1 (view _1 structure) structure
  putStrLn "Law 3"
  putStrLn $ show $ set _1 value2 (set _1 value1 structure)
  putStrLn $ show $ set _1 value2 structure
  putStrLn $
    show $ set _1 value2 (set _1 value1 structure) == (set _1 value2 structure)
