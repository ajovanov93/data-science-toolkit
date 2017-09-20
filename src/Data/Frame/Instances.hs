module Data.Frame.Instances where

-- Enum instances for product types using bit packing 

import Data.Bits()

-- We need this to allow grouping with columns that contain missing values
-- Nothing is a special group 0 while Just values are a group each.
instance (Enum a) => Enum (Maybe a) where
  fromEnum Nothing  = 0
  fromEnum (Just x) = 1 + (fromEnum x)

  toEnum 0 = Nothing
  toEnum x = Just $ (toEnum (x - 1))


-- TODO: Implement me so groupby can work with composite keys.
-- Packs a into high bits and b into low bits for fromEnum.
-- Unpacks for toEnum.
-- This is needed so groupby can work with composite keys.
-- Find a better and more robust way than bit packing.
-- instance (Enum a, Enum b) => Enum (a, b) where
-- 	fromEnum (x, y) = 0
-- 	toEnum v = (toEnum v, toEnum v)

-- Actually this led me to the following question.
-- Assume there are fromEnum instances that yield Z+ from types A and B.
-- Look for a way to define fromEnum for (A, B) so
-- Find an operator * :: Z+ -> Z+ -> Z+ such that the following holds.
-- Let F be a family of partially applied functions from the left F = {1*, 2*, 3*, ...} (We ignore 0 because 0 * x = 0)
-- forall (F f1, F f2, Z+ n, Z+ k) f1(n) != f2(k) -> * is a good fit for projecting two element product types to int.
-- For example the above property is not satisfied by regular multiplication (* = regular positve integer multiplication).
-- If we take f1 = 3* and f2 = 6* then f1 4 = 12 = f2 2 so there is no way to know if A = 3 and B = 4 or A = 6 and B = 2.
-- The goal is to have a unique as defined mapping from (A, B) to Z+ via (toEnum A * toEnum B)
