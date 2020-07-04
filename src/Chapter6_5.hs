{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6_5 where

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

chapter6_5 :: IO ()
chapter6_5
{-Exercise 1-}
 = do
  putStrLn $ ["Yer", "a", "wizard", "Harry"] ^.. folded . folded
  putStrLn . show $ [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
  putStrLn . show $ [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
  putStrLn . show $ ["bob", "otto", "hannah"] ^.. folded . to reverse
  putStrLn . show $ ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse .
    folded
{-Exercise 2-}
  putStrLn . show $ [1 .. 5] ^.. folded . to ((*) 100)
  putStrLn . show $ (1, 2) ^.. each
  putStrLn . show $ [(1, "one"), (2, "two")] ^.. folded . folded
  putStrLn . show $ (Just 1, Just 2, Just 3) ^.. each . folded
  putStrLn . show $ [Left 1, Right 2, Left 3] ^.. folded . folded
  putStrLn . show $ [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . each .
    folded
  putStrLn . show $ [1, 2, 3, 4] ^.. folded .
    to
      (\n ->
         if odd n
           then Left n
           else Right n)
  putStrLn . show $ [(1, (2, 3)), (4, (5, 6))] ^.. folded .
    to (\(a, (b, c)) -> [a, b, c]) .
    folded
  putStrLn . show $ [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded .
    to (\(a, b) -> a ^.. folded ++ b ^.. folded) .
    folded
  putStrLn . show $ [(1, "one"), (2, "two")] ^.. folded .
    to (\(a, b) -> [Left a, Right b]) .
    folded
  putStrLn . show $ S.fromList ["apricots", "apples"] ^.. folded . to reverse .
    folded
{-Exercise 3-}
  putStrLn . show $ [(12, 45, 66), (91, 123, 87)] ^.. folded .
    to (\(_, a, _) -> show a) .
    to reverse .
    folded
  putStrLn . show $ [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded .
    to
      (\(a, b) ->
         if odd a
           then Nothing
           else Just b) .
    folded
