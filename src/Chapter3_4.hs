{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3_4 where

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
record :: Lens' ([a], a) a
record = lens getter setter
  where
    getter (_, a) = a
    setter (xs, x) y = (x : xs, y)

{-Exercise 2-}
data Err
  = ReallyBadError
      { _msg :: String
      }
  | ExitCode
      { _code :: Int
      }
  deriving (Eq, Show)

msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    setMsg (ExitCode n) newMessage = ExitCode n

{-Exercise 3-}
msgv2 :: Lens' Err String
msgv2 = lens getMsg setMsg
  where
    getMsg (ReallyBadError s) = s
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) s = ReallyBadError s
    setMsg (ExitCode _) s = ReallyBadError s

chapter3_4 :: IO ()
chapter3_4 = do
  putStrLn $ show $ set msg (view msg (ExitCode 5)) (ExitCode 5) == ExitCode 5
  putStrLn $
    show $
    set msg (view msg (ReallyBadError "Bruh")) (ReallyBadError "Bruh") ==
    ReallyBadError "Bruh"
  putStrLn $
    show $
    set msg "Bruh" (set msg "Bruh" (ReallyBadError "Hi")) ==
    ReallyBadError "Bruh"
  putStrLn $ show $ set msg "Bruh" (set msg "Bruh" (ExitCode 5)) == ExitCode 5
