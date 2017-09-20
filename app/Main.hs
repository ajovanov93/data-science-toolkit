{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Labels
import Labels.Cassava.Instances()
import Data.Csv(FromField, ToField, parseField, toField)

import Data.Frame
import Data.Frame.Extras

import Data.Frame.Plot
import Graphics.Rendering.Chart.Easy (red, blue)

import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Statistics.Sample as S

import Control.Arrow((&&&))

data Variety = Setosa | Versicolor | Virginica deriving (Show, Enum, Eq, Ord)

instance FromField Variety where
  parseField "Setosa"     = pure Setosa
  parseField "Versicolor" = pure Versicolor
  parseField "Virginica"  = pure Virginica
  parseField _            = pure Setosa

instance ToField Variety where
  toField Setosa     = "Setosa"
  toField Versicolor = "Versicolor"
  toField Virginica  = "Virginica"


type IrisType = ("sepal.length" := Double, "sepal.width" := Double,
                 "petal.length" := Double, "petal.width" := Double,
                 "variety"      := Variety)

main :: IO ()
main = do
  iris <- readCsv "data/iris.csv" :: IO (V.Vector IrisType)
  let cls = iris `p` #variety;
      x   = iris `p` $("sepal.length");
      y   = iris `p` $("sepal.width");

      -- Generate a new column
      -- iris' = iris `gen` (\r -> $("sepal.area") := (*) (get $("sepal.length") r) (get $("sepal.width") r));
      -- Nicer but too many brackets
      iris' = iris `gen` (($("sepal.area") :=) . uncurry (*) . ((&&&) (get $("sepal.length")) (get $("sepal.width"))))

      metadata = (PlotMetadata "data/plot.png" "My very first plot in Haskell ggplot")  
      aes1     = Aes iris $("sepal.length") $("sepal.width") (#variety, \_ -> red) -- due to GTL.KnownSymbol constraint a label must be present :/
      aes2     = Aes iris $("petal.length") $("petal.width") (#variety, \_ -> blue) -- even if using constant colours

  ggplot (Plot metadata
               [(aes1, Point "Scatter plot for sepal"),
                (aes2, Point "Scatter plot for petal")
               ]
         )
  
  --iris `writeCsv` "data/iris_out.csv"

  print "Showcase of some functions in data science toolkit"

  print "Sepal length projection"
  printDF x

  print "Average sepal length"
  print $ S.mean (iris `p` $("sepal.length"))

  print "With new column added"
  printDF iris'

  print "Histogram of variety"
  print $ discreteHistogram iris #variety

  putStrLn "Goodbye  and have a nice day :)"
