-- Copyright (C) Aleksandar Jovanov 2017
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- The following things need to be added to Labels so DST can progress
-- > drop field from record = drop #age (#age := 1, #blabla := ":)") gives (#blabla := ":)"), project can do this but it requires a type anotation
-- > append two records     = append (#age := 1, #bla := ")") (#name := "User", #lbl := "Hey there") 
--                            gives (#age := 1, #bla := ")", #name := "User", #lbl := "Hey there")
-- > easy type casts        = cast #value @Integer (#value := 1.3, #test = 1.5) = (#value := 1, #test = 1.5) 
--                            or change modify (value -> value) to (value1 -> value2)

-- They hold back joins, binData, mergeColumns and dropColumns
-- Labels.Cassava.Instances needs ToNamedRecord instances for writeCsv

-- TODO: Finish simple ggplot implementation

-- Requires changes to Labels by its author.
-- TODO: Fix writing to csv by implementing ToNamedRecord
-- TODO: Add joins (discrimination package in backend)

module Data.Frame (
  p, -- Works
  pl, -- Works
  gen, -- Works
  prep, -- Works
  apply, -- Works
  rowcat, -- Works
  groupBy, -- Works
  sortBy, -- Works
  sortByD, -- Works
  ParseException,
  readCsv, -- Works
  EmptyDataFrameWriteException,
  writeCsv, -- FIXME: Does not work because there are no labels for ToNamedRecord instances
  printDF -- Works
) where

-- Labels
import Labels
import Data.Proxy(Proxy)
import qualified Labels.Internal as LI
import qualified GHC.TypeLits as GTL

-- Reading csv
import Data.Csv (FromNamedRecord, ToNamedRecord,
                 decodeByName, encodeByName)
import Control.Exception(throwIO, Exception)
import Labels.Cassava.Instances() 

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

-- DataFrame is actually a vector
import qualified Data.Vector as V

import qualified Data.List as L

-- For groupby
import qualified Data.Discrimination as D

import Data.Function(on)

import Data.Frame.Instances()

-- Project a column from a data frame to get a vector of values.
-- Usage example: @df `p` #age@ gives V.fromList [1, 5, 13, 8...]
p :: Has label b record => V.Vector record -> Proxy label -> V.Vector b
df `p` colName = V.map (get colName) df

-- Project a column from a dataframe tp get a new one column dataframe.
-- Usage example: @df `pl` #age@ gives V.fromList [#age := 1, #age := 5, ...]
pl :: (Has label value record, GTL.KnownSymbol label) =>
       V.Vector record -> Proxy label -> V.Vector (label := value)
df `pl` colName = V.map (\r -> colName := get colName r) df

-- Generate a new column from other columns.
-- Usage example: @df `gen` \row -> (#age := (get #yearcurrent row) - (get #yearborn row))@
gen :: Cons label value record =>
     V.Vector record
     -> (record -> label := value)
     -> V.Vector (LI.Consed label value record)
df `gen` f = V.map (\r -> LI.cons (f r) r) df

-- Prepends a ready made column with a label to a dataframe.
-- The intended usage for this function is when interacting with other libraries.
-- Safe method. (TODO: Make even safer with Dependant Types)
-- Cuts the column length to number of rows of dataframe and never overflows.
-- Returns an empty dataframe if an empty dataframe is passed as a result so never underflows.
-- Usage example: @(#age := V.replicate (V.length df) 22) `prep` df@
prep :: Cons label value record =>
       (label := V.Vector value) -> V.Vector record ->  V.Vector (LI.Consed label value record)
(colName := vals) `prep` df = V.imap (\i row -> LI.cons (colName := (vals V.! i)) row) df


-- Mapping over a column simplified a bit.
apply  :: Has label t b => V.Vector b -> Data.Proxy.Proxy label -> (t -> t) -> V.Vector b
apply df colName f = V.map (\r -> set colName (f (get colName r)) r) df

-- Concatenates two dataframes, one after the other in relation to rows.
-- Their dimensions in number of columns must match or this will fail at compile time.
rowcat :: V.Vector record -> V.Vector record -> V.Vector record
df1 `rowcat` df2 = df1 V.++ df2

-- To groupby with a composite key generate a new column using (,) tuple constructor and gen first.
-- FIXME: There are no instances for Enum for product types. 
groupBy :: (Enum value, LI.Has colName value record, GTL.KnownSymbol colName) => 
            V.Vector record -> Proxy colName -> [(value, V.Vector record)]
groupBy df colName = zip hdr grp'
  where
    grp  = D.groupWith (fromEnum . get colName) (V.toList df)
    hdr  = map (get colName . head) grp
    grp' = map V.fromList grp

-- Comparison based sort O(n log(n))
sortBy  :: (Ord value, LI.Has colName value record, GTL.KnownSymbol colName) => 
           V.Vector record -> Proxy colName -> V.Vector record
sortBy df colName = (V.fromList . L.sortBy (compare `on` (get colName)) . V.toList) df

-- Sorting with discrimination O(n)
sortByD :: (Enum value, LI.Has colName value record, GTL.KnownSymbol colName) => 
           V.Vector record -> Proxy colName -> V.Vector record
sortByD df colName = (V.fromList . D.sortWith (fromEnum . get colName) . V.toList) df 

newtype ParseException = ParseException String deriving Show

instance Exception ParseException

readCsv :: (FromNamedRecord record) => FilePath -> IO (V.Vector record)
readCsv path = do
  f <- BL.readFile path -- rethrow exception or handle it here?
  case decodeByName f of
    Left err       -> throwIO $ ParseException err --return V.empty
    Right (_, vec) -> return vec


data EmptyDataFrameWriteException = EmptyDataFrameWriteException deriving Show
instance Exception EmptyDataFrameWriteException

writeCsv :: (ToNamedRecord a, Reflect Show a) => V.Vector a -> FilePath -> IO ()
writeCsv df path =
  if V.null df
  then throwIO EmptyDataFrameWriteException
  else BL.writeFile path bs
  where
      lbl = map fst $ reflect @Show show (V.head df)
      hdr = V.fromList $ map BS.pack lbl
      bs  = encodeByName hdr (V.toList df)


printDF :: (Show a) => V.Vector a -> IO ()
printDF = V.mapM_ print