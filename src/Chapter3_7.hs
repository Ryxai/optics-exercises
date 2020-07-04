{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_7 where

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
    , _kelvin :: Float
    }
  deriving (Show)

makeLenses ''Temperature

celsius :: Lens' Temperature Float
celsius = lens getter setter
  where
    getter = (subtract 273.15) . view kelvin
    setter temp c = set kelvin (c + 273.15) temp

{-Exercise 1-}
data User =
  User
    { _firstName :: String
    , _lastName :: String
    , _email :: String
    }
  deriving (Show)

makeLenses ''User

username :: Lens' User String
username = lens getter setter
  where
    getter = (takeWhile ((/= '@')) . view email)
    setter user uname =
      set email (uname ++ (dropWhile (/= '@') (view email user))) user

{-Exercise 2-}
fullName :: Lens' User String
fullName = lens getter setter
  where
    getter user = (view firstName user) ++ " " ++ (view lastName user)
    setter user name =
      set firstName (takeWhile (/= ' ') name) $
      set lastName (drop 1 (dropWhile (/= ' ') name)) user

user = User {_firstName = "Tim", _lastName = "Tim", _email = "tim@tim.tim"}

jc = User "John" "Cena" "invisible@example.com"

chapter3_7 :: IO ()
chapter3_7 = do
  putStrLn $ show $ set username "tom" user
  putStrLn $ view fullName jc
  putStrLn $ show $ set fullName "Doctor of Thuganomics" jc
