module Data.Frame.Join where

import qualified Data.Vector as V

import qualified Data.Discrimination as D
import qualified Data.Discrimination.Grouping as DG

-- Theoretically will exist one day
-- import Labels(get, recordAppend)

-- discriminator = DG.hashing

-- https://github.com/Gabriel439/slides/blob/master/lambdaconf/data/exercises/08/Main.hs
-- https://hackage.haskell.org/package/discrimination-0.2.1/docs/Data-Discrimination.html
-- D.inner (DG.hashing) (\(a, b) (c, d) -> (a, b, c, d)) fst snd [(1, 2), (2, 3)] [(5, 1), (3, 2)]

-- innerJoin df1 df2 lbl1 lbl2 = V.fromList . head $ D.inner discriminator combiner p1 p2 dat1 dat2
-- 	where
-- 		combiner = \r1 r2 -> recordAppend r1 r2
-- 		p1       = get lbl1
-- 		p2       = get lbl2
-- 		dat1     = V.toList df1
-- 		dat2     = V.toList df2