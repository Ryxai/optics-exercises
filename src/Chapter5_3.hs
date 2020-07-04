{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter5_3 where

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

{- Exercise 2-}
chapter5_3 :: IO ()
chapter5_3 = do
  putStrLn . show $ (False, "opossums") & _1 ||~ True
  putStrLn . show $ 2 & id *~ 3
  putStrLn . show $ ((True, "Dudley"), 55.0) & _1 . _2 <>~ " - the worst" & _2 -~
    15 &
    _2 //~
    2 &
    _1 .
    _2 %~
    map toUpper &
    _1 .
    _1 &&~
    False
{-Exercise 3: &-}
{-Exercise 4: Lens s t a b -> (a -> b) -> s -> t-}
