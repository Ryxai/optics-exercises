{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter5_2 where

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

data Gate =
  Gate
    { _open :: Bool
    , _oilTemp :: Float
    }
  deriving (Show)

data Army =
  Army
    { _archers :: Int
    , _knights :: Int
    }
  deriving (Show)

data Kingdom =
  Kingdom
    { _name :: String
    , _army :: Army
    , _gate :: Gate
    }
  deriving (Show)

makeLenses ''Gate

makeLenses ''Army

makeLenses ''Kingdom

duloc :: Kingdom
duloc =
  Kingdom
    { _name = "Duloc"
    , _army = Army {_archers = 22, _knights = 14}
    , _gate = Gate {_open = True, _oilTemp = 10.0}
    }

chapter5_2 :: IO ()
chapter5_2 = do
  putStrLn . show $ duloc & name <>~ ": a perfect place" & army . knights +~ 30 &
    gate .
    open &&~
    False
  putStrLn . show $ duloc & name <>~ "instein" & army . archers -~ 5 & army .
    knights +~
    12 &
    gate .
    oilTemp +~
    90
  putStrLn . show $ duloc & gate . oilTemp -~ 5.0 & name <>~ ": Home" & name <<<>~
    " of the talking Donkeys"
