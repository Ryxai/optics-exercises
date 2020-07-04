{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter5_1 where

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

data Payload =
  Payload
    { _weightKilos :: Int
    , _cargo :: String
    }
  deriving (Show)

makeLenses ''Payload

data Ship =
  Ship
    { _payload :: Payload
    }
  deriving (Show)

makeLenses ''Ship

serenity :: Ship
serenity = Ship (Payload 50000 "Livestock")

data Thermometer =
  Thermometer
    { _temperature :: Int
    }
  deriving (Show)

makeLenses ''Thermometer

chapter5_1 :: IO ()
chapter5_1 = do
  putStrLn . show $ view (payload . cargo) serenity
  putStrLn . show $ serenity ^. payload . cargo
  putStrLn . show $ set (payload . cargo) "Medicine" serenity
  putStrLn . show $ serenity & payload . cargo .~ "Medicine"
  putStrLn . show $ serenity & payload . cargo .~ "Chocolate" & payload .
    weightKilos .~
    2310
  putStrLn . show $ serenity & payload . weightKilos %~ subtract 1000 & payload .
    cargo .~
    "Chocolate"
  putStrLn . show $ (2, 30) & _2 +~ 5
  putStrLn . show $ (2, 30) & _2 //~ 2
  putStrLn . show $ (2, 30) & _1 *~ 3
  putStrLn . show $ (False, 30) & _1 ||~ True
  putStrLn . show $ ("abra", 30) & _1 <>~ "cadabra"
  putStrLn . show $ Thermometer 20 & temperature <+~ 15
  putStrLn . show $ Thermometer 20 & temperature <<+~ 15
  putStrLn . show $
    map (view _2) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
  putStrLn . show $
    map (^. _2) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
  putStrLn . show $
    map (over _2 reverse) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
  putStrLn . show $
    map (_2 %~ reverse) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
