{-# LANGUAGE  FlexibleContexts, GADTs #-}

module Data.Frame.Plot where

-- Very experimental, sudden API changes might happen, Chart might be exchanged for Diagrams, etc.

-- Data frame
import Data.Frame (p)
import qualified  Data.Vector as V

-- Labels & Type system
import Data.Proxy
import GHC.OverloadedLabels
import qualified Labels.Internal as LI
import qualified GHC.TypeLits as GTL

-- Actual ploting functionality
import Graphics.Rendering.Chart.Easy (def, layout_title, (.=), setColors,
                                      plot, line, points, 
                                      Colour, opaque)

import Graphics.Rendering.Chart.Backend.Cairo (toFile)

-- TODO: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

-- output file path and plot title
data PlotMetadata where 
    PlotMetadata :: FilePath -> -- where to write the plot image 
                    String   -> -- global plot title
                    PlotMetadata

data Aesthetics where
    Aes :: (Num a, Num b, -- X and Y coordinate values must be numbers
            LI.Has colX a record, LI.Has colY b record, LI.Has colColor c record, -- The row/record must contain the column names
            GTL.KnownSymbol colX, GTL.KnownSymbol colY, GTL.KnownSymbol colColor) =>
           V.Vector record -> -- the data frame 
           Proxy colX -> -- the column from which to draw the x values
           Proxy colY -> -- the column from which to draw the y values 
           (Proxy colColor, c -> Colour Double) -> -- column to draw values from and function to map these values to a colour
           Aesthetics

type GlobalPlotTitle = String

data Geometry where
    Line  :: GlobalPlotTitle -> Geometry
    Point :: GlobalPlotTitle -> Geometry

    -- Area      :: String -> Geometry
    -- Histogram :: String -> Geometry
    -- Density   :: String -> Geometry

    -- Bar       :: String -> Geometry

-- Single axis, multi curve/histogram plot
data GGPlot where
    Plot :: PlotMetadata -> [(Aesthetics, Geometry)] -> GGPlot

-- Currently only a single geom and aes pair are supported and single color
-- ggplot :: GGPlot -> IO ()
-- ggplot (Plot _ []) = return ()
-- ggplot (Plot metadata plotData) = toFile def filePath $ do
--     layout_title .= plotTitle
    
--     plotOne (head plotData)
    
--     where
--         plotOne (Aes df colX colY (colColour, v2c), geom) =
--             do
--                 setColors [opaque colour]
--                 case geom of
--                     Line title  -> plot (line title [signal])
--                     Point title -> plot (points title signal)
--             where
--                 -- Kinda cheating here with only one colour...
--                 colour       = V.head   $ V.map v2c (df `p` colColour)
--                 signal       = V.toList $ V.zip (df `p` colX) (df `p` colY) 

--         PlotMetadata filePath plotTitle = metadata

ggplot :: GGPlot -> IO ()
ggplot _ = putStrLn "ggplot is not yet implemented"