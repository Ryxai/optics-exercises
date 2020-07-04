{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6_4 where

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

data Name =
  Name
    { getName :: String
    }
  deriving (Show)

data ShipCrew =
  ShipCrew
    { _shipName :: Name
    , _captain :: Name
    , _firstMate :: Name
    , _conscripts :: [Name]
    }
  deriving (Show)

makeLenses ''ShipCrew

collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers crew = [_captain crew, _firstMate crew] ++ _conscripts crew

crewMembers :: Fold ShipCrew Name
crewMembers = folding collectCrewMembers

myCrew :: ShipCrew
myCrew =
  ShipCrew
    { _shipName = Name "Purple Pearl"
    , _captain = Name "Grumpy Roger"
    , _firstMate = Name "Long John Bronze"
    , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
    }

crewNames :: Fold ShipCrew Name
crewNames =
  folding (\s -> s ^.. captain <> s ^.. firstMate <> s ^.. conscripts . folded)

chapter6_4 :: IO ()
chapter6_4 = do
  putStrLn . show $ myCrew ^.. crewMembers
  putStrLn $ Name "Two-faced Tony" ^. to getName
  putStrLn $ Name "Two-faced Tony" ^. to getName . to (fmap toUpper)
  putStrLn $ Name "Two-faced Tony" ^. to (fmap toUpper . getName)
  putStrLn . show $ myCrew ^.. crewMembers . to getName
  putStrLn . show $ myCrew ^.. crewNames . to getName
