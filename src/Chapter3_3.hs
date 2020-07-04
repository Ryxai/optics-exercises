{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_3 where

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

type UserName = String

type UserId = String

data Session =
  Session
    { _userId :: UserId
    , _userName :: UserName
    , _createdTime :: String
    , _expiryTime :: String
    }
  deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

alongsideUserId :: Lens' Session (Session, UserId)
alongsideUserId = lensProduct id userId

session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"

newSession = session {_userId = "USER-5678"}

chapter3_3 :: IO ()
chapter3_3 = do
  putStrLn $ show $ view userInfo session
  putStrLn $
    show $
    view alongsideUserId (set alongsideUserId (newSession, "USER-9999") session)
  putStrLn $
    show $
    (view
       alongsideUserId
       (set alongsideUserId (newSession, "USER-9999") session)) ==
    (newSession, "USER-9999")
