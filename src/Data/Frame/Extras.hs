{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Frame.Extras 
    (discreteHistogram,
     binData)
where

import Labels(get, set)
import qualified Labels.Internal as LI
import qualified GHC.TypeLits    as GTL
import Data.Proxy

import Data.Frame (groupBy, p)
import Control.Arrow(second)

import qualified Data.Vector as V
import qualified Data.List   as L 

-- @Usage: discreteHistogram iris #variety = [(Setosa, 50), (Versicolor, 50), (Virginica, 50)]@ 
discreteHistogram :: (Enum value, LI.Has colName value record, GTL.KnownSymbol colName) => 
                      V.Vector record -> Proxy colName -> [(value, Int)]
discreteHistogram df colName = map (second V.length) (groupBy df colName)
     
-- Assumes ranges with no overlap.
-- Will produce unwanted behaviour if ranges overlap.
-- Maybe write liquid haskell anotations to check this holds.  
-- FIXME: The bins which should be integer are written as (Num q).
binData :: (LI.Has colName q record, GTL.KnownSymbol colName, 
            Num q, Ord q, Fractional q) => 
            V.Vector record -> Proxy colName -> [(q, q)] -> V.Vector record
binData df colName ranges = V.imap (\i r -> set colName (binned V.! i) r) df
    where
        mn      = L.minimum $ map fst ranges
        mx      = L.maximum $ map snd ranges 
        inf     = 1E200 -- Type system issues :) (Warning this might trigger the error below in some obscure cases, fixme)
        -- Include extra ranges for the outer left towards -inf and outer right towards +inf
        ranges' = (-inf, mn) : ranges ++ [(mx, inf)]
        
        -- Find range function
        fr :: (Num q, Ord q) => q -> [(q, q)] -> q -> q
        fr _ [] _ = error "This should never happen!"
        fr x (r:rs) i
            | x >= (fst r) && x < (snd r)  = i
            | otherwise                    = fr x rs (i + 1)  

        binned = V.map (\x -> fr x ranges' 0) (df `p` colName)

-- Boilerplate needed for the dropNA function
class IsMaybe a where
    isJust :: a -> Bool
instance IsMaybe (Maybe a) where
    isJust (Just _) = True
    isJust Nothing  = False

-- Drop rows where column colName has missing values
dropNA :: (IsMaybe value, LI.Has colName value record, GTL.KnownSymbol colName) => 
           V.Vector record -> Proxy colName -> V.Vector record
dropNA df colName = V.filter (isJust . get colName) df