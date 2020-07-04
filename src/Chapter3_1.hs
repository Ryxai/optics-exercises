{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_1 where

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

data Ship =
  Ship
    { _name :: String
    , _numCrew :: Int
    }
  deriving (Show)

makeLenses ''Ship

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newNumCrew = ship {_numCrew = newNumCrew}

numCrewLens :: Lens' Ship Int
numCrewLens = lens getNumCrew setNumCrew

getName :: Ship -> String
getName = _name

setName :: Ship -> String -> Ship
setName ship newName = ship {_name = newName}

nameLens :: Lens' Ship String
nameLens = lens getName setName

purplePearl :: Ship
purplePearl = Ship {_name = "Purple Pearl", _numCrew = 30}

chapter3_1 :: IO ()
chapter3_1 = do
  putStrLn $ show $ view numCrew purplePearl
  putStrLn $ show $ over numCrew (+ 3) purplePearl
  putStrLn $ show $ view numCrewLens purplePearl
  putStrLn $ show $ over numCrewLens (+ 3) purplePearl
