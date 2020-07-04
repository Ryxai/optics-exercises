{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6_3 where

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

{-Exercise 1-}
{-Exercise 2-}
{-2.a.1 folded :: Fold [(Int, Char)] (Int, Char)-}
{-2.a.2 _2 :: Fold (Int, Char) Int-}
{-2.b.1 toListOf :: Fold (Bool, Set String) [String] -> (Bool, Set String) -> [String] -}
{-2.b.2 _2 :: Fold (Bool, Set String) Set String-}
{-2.b.3 folded :: Fold (Set String) String-}
{-2.c.1 folded :: Fold (M.Map String, String) String-}
{-2.c.2 folded :: Fold (String, String) Char-}
{-2.c.3. toListOf :: Fold (M.Map String, String) Char -> M.Map String, String -> String-}
{-Exercise 3-}
chapter6_3 :: IO ()
chapter6_3 = do
  putStrLn . show $ [1, 2, 3] ^.. folded
  putStrLn . show $ ("Light", "Dark") ^.. _1
  putStrLn . show $ [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . each
  putStrLn . show $ [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
  putStrLn . show $ [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _2 .
    folded
  putStrLn . show $ ("Bond", "James", "Bond") ^.. each
