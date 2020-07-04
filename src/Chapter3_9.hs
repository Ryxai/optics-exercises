{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_9 where

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

{-Exercise 1 + 2 -}
data ProductPrices =
  ProductPrices
    { _limePrice :: Float
    , _lemonPrice :: Float
    }
  deriving (Show)

boundedRound :: Float -> Float -> Float -> Float
boundedRound val target bound
  | val < target = max val (target - bound)
  | otherwise = min val (target + bound)

limePrice :: Lens' ProductPrices Float
limePrice = lens getter setter
  where
    getter (ProductPrices p _) = p
    setter (ProductPrices _ p) price =
      (\np -> ProductPrices np (boundedRound p np 0.5)) $ (max price 0)

lemonPrice :: Lens' ProductPrices Float
lemonPrice = lens getter setter
  where
    getter (ProductPrices _ p) = p
    setter (ProductPrices p _) price =
      (\np -> ProductPrices (boundedRound p np 0.5) np) $ (max price 0)

prices = ProductPrices 1.50 1.48

chapter3_9 :: IO ()
chapter3_9 = do
  putStrLn . show $ set limePrice 2 prices
  putStrLn . show $ set limePrice 1.8 prices
  putStrLn . show $ set limePrice 1.63 prices
  putStrLn . show $ set limePrice (-1.0) prices
