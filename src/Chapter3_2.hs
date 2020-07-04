{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_2 where

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

data Err
  = ReallyBadError
      { _msg :: String
      }
  | ExitCode
      { _code :: Int
      }

msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    setMsg (ExitCode n) newMessage = ExitCode n

newMessage = "False Alarm!"

chapter3_2 :: IO ()
chapter3_2 = do
  putStrLn $ view msg (set msg newMessage (ReallyBadError "BAD BAD BAD"))
  putStrLn $
    show $
    view msg (set msg newMessage (ReallyBadError "BAD BAD BAD")) == newMessage
  putStrLn $ view msg (set msg newMessage (ExitCode 1))
  putStrLn $ show $ view msg (set msg newMessage (ExitCode 1)) == newMessage
