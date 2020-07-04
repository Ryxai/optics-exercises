{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6_6 where

import Control.Applicative
import Control.Lens
import Control.Lens.Unsound
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char
import qualified Data.Map as M
import Data.Monoid
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T

data Actor =
  Actor
    { _name :: String
    , _birthYear :: Int
    }
  deriving (Show, Eq)

data TVShow =
  TVShow
    { _title :: String
    , _numEpisodes :: Int
    , _numSeasons :: Int
    , _criticScore :: Double
    , _actors :: [Actor]
    }
  deriving (Eq, Show)

makeLenses ''Actor

makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother =
  TVShow
    { _title = "How I Met Your Mother"
    , _numEpisodes = 208
    , _numSeasons = 9
    , _criticScore = 83
    , _actors =
        [ Actor "Josh Radnor" 1974
        , Actor "Cobie Smulders" 1982
        , Actor "Neil Patrick Harris" 1973
        , Actor "Alyson Hannigan" 1974
        , Actor "Jason Segel" 1980
        ]
    }

buffy :: TVShow
buffy =
  TVShow
    { _title = "Buffy the Vampire Slayer"
    , _numEpisodes = 144
    , _numSeasons = 7
    , _criticScore = 81
    , _actors =
        [ Actor "Sarah Michelle Geller" 1977
        , Actor "Alyson Hannigan" 1974
        , Actor "Nicholas Brendon" 1971
        , Actor "David Boreanaz" 1969
        , Actor "Anthony Head" 1954
        ]
    }

tvShows :: [TVShow]
tvShows = [howIMetYourMother, buffy]

comparingOf :: Ord a => Lens' s a -> s -> s -> Ordering
comparingOf l = comparing $ view l

calcAge :: Actor -> Int
calcAge actor = 2030 - _birthYear actor

showActor :: Actor -> String
showActor actor = _name actor <> ": " <> show (calcAge actor)

ageSummary :: Actor -> (Sum Int, Sum Int)
ageSummary actor = (Sum 1, Sum (calcAge actor))

computeAverage :: (Sum Int, Sum Int) -> Double
computeAverage (Sum count, Sum total) = fromIntegral total / fromIntegral count

chapter6_6 :: IO ()
chapter6_6 = do
  putStrLn . show $ sumOf (folded . numEpisodes) tvShows
  putStrLn . show $ maximumOf (folded . criticScore) tvShows
  putStrLn . show $
    _title <$> maximumByOf folded (comparing _criticScore) tvShows
  putStrLn . show $
    minimumByOf (folded . actors . folded) (comparing _birthYear) tvShows
  putStrLn . show $
    minimumByOf (folded . actors . folded) (comparingOf birthYear) tvShows
  traverseOf_ (folded . actors . folded . to showActor) putStrLn tvShows
  putStrLn . show $
    execState (traverseOf_ folded (modify . const (+ 1)) tvShows) 0
  putStrLn . show $ foldOf (folded . actors . folded . to ageSummary) tvShows
  putStrLn . show $
    computeAverage $ foldOf (folded . actors . folded . to ageSummary) tvShows
  putStrLn . show $
    computeAverage $ foldMapOf (folded . actors . folded) ageSummary tvShows
  putStrLn $ Just "do it" ^. folded
  putStrLn $ (Nothing ^. folded :: String)
  putStrLn $ ("one", "two", "three") ^. each
  putStrLn . show $
    foldMapOf (folded . actors . folded . name) (\n -> M.singleton n 1) tvShows
  putStrLn . show $ M.singleton 'a' "first" <> M.singleton 'a' "second"
  putStrLn . show $
    M.unionWith (+) (M.singleton "an actor" 1) (M.singleton "an actor" 1)
  putStrLn . show $
    foldMapByOf
      (folded . actors . folded . name)
      (M.unionWith (+))
      mempty
      (\n -> M.singleton n 1)
      tvShows
  {-Exercise 1-}
  putStrLn . show $ has folded []
  putStrLn . show $ foldOf each ("Yo", "Adrian")
  putStrLn . show $ elemOf each "phone" ("E.T.", "phone", "home")
  putStrLn . show $ minimumOf folded [5, 7, 2, 3, 13, 17, 11]
  putStrLn . show $ lastOf folded [5, 7, 2, 3, 13, 17, 11]
  putStrLn . show $
    anyOf ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
