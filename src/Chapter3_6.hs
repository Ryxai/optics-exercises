{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_6 where

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

data Temperature =
  Temperature
    { _location :: String
    , _celsius :: Float
    }
  deriving (Show)

makeLenses ''Temperature

temp = Temperature "Berlin" 7.0

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9 / 5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter temp f = set celsius (fahrenheitToCelsius f) temp

chapter3_6 :: IO ()
chapter3_6 = do
  putStrLn $ show $ view celsius temp
  putStrLn $ show $ set celsius 13.5 temp
  putStrLn $ show $ over celsius (+ 10) temp
  putStrLn $ show $ set celsius (fahrenheitToCelsius 56.3) temp
  putStrLn $
    show $
    over celsius (fahrenheitToCelsius . (+ 18) . celsiusToFahrenheit) temp
  putStrLn $ show $ view fahrenheit temp
  putStrLn $ show $ over fahrenheit (+ 18) temp
