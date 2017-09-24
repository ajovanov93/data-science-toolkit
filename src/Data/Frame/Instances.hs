module Data.Frame.Instances where

-- We need this to allow grouping with columns that contain missing values
-- Nothing is a special group 0 while Just values are a group each.
instance (Enum a) => Enum (Maybe a) where
  fromEnum Nothing  = 0
  fromEnum (Just x) = succ . fromEnum $ x

  toEnum 0 = Nothing
  toEnum x = Just . toEnum . pred $ x

-- We need this to allow grouping with composite keys. Composite key = tuple valued column.
-- How many unique values can each tuple component contain.
-- Valeus above constK will overflow and information will be lost.
constK = 30000

instance (Enum a, Enum b) => Enum (a, b) where
    fromEnum (a, b) = (fromEnum a) * constK + ((fromEnum b) `mod` constK)

    toEnum x        =  (a', b')
        where
            a' = toEnum (x `div` constK)
            b' = toEnum (x `mod` constK)

instance (Enum a, Enum b, Enum c) => Enum (a, b, c) where
 fromEnum (a, b, c) = fromEnum (a, b) * constK + ((fromEnum c) `mod` constK)
 toEnum   x         = (toEnum a', toEnum b', toEnum c')
     where
         ab = x  `div` constK
         c' = x  `mod` constK
         a' = ab `div` constK
         b' = ab `mod` constK

-- Actually this led me to the following question.
-- Assume there are fromEnum instances that yield Z+ from types A and B.
-- Look for a way to define fromEnum for (A, B) so
-- Find an operator * :: Z+ -> Z+ -> Z+ such that the following holds (and constK is not needed).
-- Let F be a family of partially applied functions from the left F = {0*, 1*, 2*, 3*, ...}
-- forall (F f1, F f2, Z+ n, Z+ k) f1(n) != f2(k) -> * is a good fit for projecting two element product types to int.
-- For example the above property is not satisfied by regular multiplication (* = regular positve integer multiplication).
-- If we take f1 = 3* and f2 = 6* then f1 4 = 12 = f2 2 so there is no way to know if A = 3 and B = 4 or A = 6 and B = 2.
-- The goal is to have a unique as defined mapping from (A, B) to Z+ via (toEnum A * toEnum B)
-- Overwriting mappings like hashing don't fit in.
