{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter4 where

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

data Promotion a =
  Promotion
    { _item :: a
    , _discountPercentage :: Double
    }
  deriving (Show)

item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter :: Promotion a -> a
    getter = _item
    setter :: Promotion a -> b -> Promotion b
    setter promo newItem = promo {_item = newItem}

peachPromo = Promotion "A really delicious Peach" 25.0

buffyFigurines = ["Buffy", "Angel", "Willow", "Guiles"]

buffyPromo = set item buffyFigurines peachPromo

data Preferences a =
  Preferences
    { _best :: a
    , _worst :: a
    }
  deriving (Show)

{-Exercise 1: Lens (Vorpal x) (Vorpal y) x y-}
{-Exercise 2-}
pref :: Lens (Preferences a) (Preferences b) (a, a) (b, b)
pref = lens getter setter
  where
    getter :: Preferences a -> (a, a)
    getter (Preferences best worst) = (best, worst)
    setter :: Preferences a -> (b, b) -> Preferences b
    setter _ (best, worst) = Preferences best worst

{-Exercise 3: Lens (Result a) (Result b) (Either a String) (Either b String)-}
{-Exercise 4: Yes, so long as the resultant types are in a tuple or the like
 - for the values of a and b in stab-}
{-Exercise 5-}
data Predicate a =
  Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where
    getter (Predicate f) = f
    setter _ f = Predicate f

data Person =
  Person
    { _name :: String
    , _address :: Address
    }
  deriving (Show)

data Address =
  Address
    { _streetAddress :: StreetAddress
    , _city :: String
    , _country :: String
    }
  deriving (Show)

data StreetAddress =
  StreetAddress
    { _streetNumber :: String
    , _streetName :: String
    }
  deriving (Show)

makeLenses ''Person

makeLenses ''Address

makeLenses ''StreetAddress

sherlock :: Person
sherlock =
  Person
    { _name = "S. Holmes"
    , _address =
        Address
          { _streetAddress =
              StreetAddress
                {_streetNumber = "221B", _streetName = "Baker Street"}
          , _city = "London"
          , _country = "England"
          }
    }

setStreetNumber :: String -> Person -> Person
setStreetNumber newStreetAddress person =
  let existingAddress = _address person
      existingStreetAddress = _streetAddress existingAddress
   in person
        { _address =
            existingAddress
              { _streetAddress =
                  existingStreetAddress {_streetNumber = newStreetAddress}
              }
        }

updateAddress :: (Address -> Address) -> (Person -> Person)
updateAddress modify existingPerson =
  existingPerson {_address = modify . _address $ existingPerson}

updateStreetAddress :: (StreetAddress -> StreetAddress) -> (Address -> Address)
updateStreetAddress modify existingAddress =
  existingAddress {_streetAddress = modify . _streetAddress $ existingAddress}

updateStreetNumber :: (String -> String) -> (StreetAddress -> StreetAddress)
updateStreetNumber modify existingStreetAddress =
  existingStreetAddress
    {_streetNumber = modify . _streetNumber $ existingStreetAddress}

data Player =
  Player
  deriving (Show)

data Wool =
  Wool
  deriving (Show)

data Sweater =
  Sweater
  deriving (Show)

data Item a =
  Item
    { _material :: a
    , _amount :: Int
    }
  deriving (Show)

makeLenses ''Item

weave :: Wool -> Sweater
weave Wool = Sweater

gameState :: (Player, Item Wool)
gameState = (Player, Item Wool 5)

chapter4 :: IO ()
chapter4 = do
  putStrLn . show $ over (_2 . material) weave gameState
{-Exercise 1: _2 . _1 . _2 -}
{-Exercise 2- mystery :: Lens' Eight Two -}
{-Exercise 3: Lens Platypus BabySloth Armadillo Hedgehog-}
{-Exercise 4 snajubjumwock . boowockugwup . gruggazinkoom . sinkattumblezz .
 - spuzorktrowmble . gazorlglesnatchka . baneryakoobog-}
