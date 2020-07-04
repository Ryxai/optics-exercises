{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6_2 where

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

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

{-beastSizes ^.. folded-}
{-[(3, "Sirens"), (882,"Kraken"), (92, "Ogopogo")]-}
{-beastSizes ^.. foldeed . folded-}
{-["Sirens", "Kraken", "Ogopogo"]-}
{-beastSizes ^.. folded . folded . folded-}
{-"SirensKrakenOgopogo"-}
{-beastSizes ^.. folded . _2-}
{-["Sirens", "Kraken", "Ogopogo"]-}
{-toListOf (folded . folded) [[1,2,3],[4,5,6]]-}
{-[1,2,3,4,5,6]-}
{-toListOf (folded . folded) (M.FromList [("Jack", "Captain"), ("Will", "First Mate")])-}
{-"CaptainFirst Mate"-}
{-("Hello", "It's me") ^.. both . folded-}
{-"HelloIt's me"-}
{-("Why", "So", "Serious?") ^.. each-}
{-["Why", "So", "Serious?"]-}
quotes :: [(T.Text, T.Text, T.Text)]
quotes =
  over
    (each . each)
    (T.pack)
    [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

chapter6_2 :: IO ()
chapter6_2 = do
  putStrLn . show $ beastSizes ^.. folded
  putStrLn . show $ beastSizes ^.. folded . folded
  putStrLn . show $ beastSizes ^.. folded . folded . folded
  putStrLn . show $ beastSizes ^.. folded . _2
  putStrLn . show $ toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
  putStrLn . show $
    toListOf
      (folded . folded)
      (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
  putStrLn . show $ ("Hello", "It's me") ^.. both . folded
  putStrLn . show $ ("Why", "So", "Serious?") ^.. each
