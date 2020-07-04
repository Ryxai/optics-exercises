{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6_1 where

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

data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

data CrewMember =
  CrewMember
    { _name :: String
    , _role :: Role
    , _talents :: [String]
    }
  deriving (Show, Eq, Ord)

makeLenses ''CrewMember

roster :: S.Set CrewMember
roster =
  S.fromList
      --                Name          Role         Talents
    [ CrewMember "Grumpy Roger" Gunner ["Juggling", "Arbitrage"]
    , CrewMember "Long-John Bronze" PowderMonkey ["origami"]
    , CrewMember "Salty Steve" PowderMonkey ["Charcuterie"]
    , CrewMember "One-eyed Jack" Navigator []
    ]

crewMembers :: Fold (S.Set CrewMember) CrewMember
crewMembers = folded

crewRole :: Fold CrewMember Role
crewRole = role

chapter6_1 :: IO ()
chapter6_1 = do
  putStrLn . show $ roster ^.. folded
  putStrLn . show $ CrewMember "Jerry" PowderMonkey ["Ice Cream Making"] ^..
    role
  putStrLn . show $ roster ^.. folded . role
  putStrLn . show $ ("Gemini", "Leo") ^.. both
  putStrLn . show $ Left "Albuquerque" ^.. both
  putStrLn . show $ ("Gemini", "Leo", "Libra") ^.. both
  putStrLn . show $ (1, 2, 3, 4, 5) ^.. each
  putStrLn . show $ [1, 2, 3, 4, 5] ^.. each
  putStrLn . show $ (T.pack ("Made him an offer he couldn't refuse")) ^.. each
  putStrLn . show $ (C8.pack ("Do or do not")) ^.. each
