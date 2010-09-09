---------------------------------------------------------------
-- |
-- Module      : Graphics.Rendering.HSparklines
-- Copyright   : (c) Hitesh Jasani, 2008
-- License     : BSD3
--
-- Maintainer  : Hitesh Jasani <hitesh.jasani@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Created     : 2008-02-26
-- Version     : 0.1
--
-- Sparklines implementation in Haskell.  Sparklines are
-- mini graphs inspired by Edward Tufte.
--
-- > dp :: [Float]
-- > dp = [24,21,32.3,24,15,34,43,55,57,72,74,75,73,72,55,44]
-- >
-- > make barSpark dp >>= savePngFile "bar_spark.png"
-- >
---------------------------------------------------------------

module Graphics.Rendering.HSparklines
    (
     -- * Types
     SparkOptions(..)
    ,rgb
     -- * Drawing functions
    ,make
    ,smoothSpark
    ,barSpark
    -- * Saving functions
    ,savePngFile
    ,encodePngAsDataUrl
    ) where


import Codec.Binary.Base64 ( encode )
import Control.Monad
import Data.ByteString ( unpack )
import Data.List
import Data.Ord
import Graphics.GD



make :: SparkOptions -> [Float] -> IO Image
make so@(SmoothOptions {}) dp = renderSmooth so dp
make so@(BarOptions {})    dp = renderBar so dp


data SparkOptions = SmoothOptions
  {
   step :: Int                  -- ^ step size
  ,height :: Int                -- ^ graph height (pixels)
  ,limits :: (Int,Int)          -- ^ data point limits
  ,bgColor :: Color             -- ^ background color
  ,minColor :: Color            -- ^ color of minimum datapoint
  ,maxColor :: Color            -- ^ color of maximum datapoint
  ,lastColor :: Color           -- ^ color of last datapoint
  ,minMarker :: Bool            -- ^ display minimum marker
  ,maxMarker :: Bool            -- ^ display maximum marker
  ,lastMarker :: Bool           -- ^ display last marker
  }
                  | BarOptions
  {
   step :: Int                  -- ^ step size
  ,height :: Int                -- ^ graph height (pixels)
  ,limits :: (Int,Int)          -- ^ data point limits
  ,bgColor :: Color             -- ^ background color
  ,minColor :: Color            -- ^ color of minimum datapoint
  ,maxColor :: Color            -- ^ color of maximum datapoint
  ,lastColor :: Color           -- ^ color of last datapoint
  ,minMarker :: Bool            -- ^ display minimum marker
  ,maxMarker :: Bool            -- ^ display maximum marker
  ,lastMarker :: Bool           -- ^ display last marker
  }
                  deriving (Show)


smoothSpark :: SparkOptions
smoothSpark = SmoothOptions
  {
   step = 2
  ,height = 20
  ,limits = (0,100)
  ,bgColor = white
  ,minColor = red
  ,maxColor = green
  ,lastColor = blue
  ,minMarker = True
  ,maxMarker = True
  ,lastMarker = True
  }


barSpark :: SparkOptions
barSpark = BarOptions
  {
   step = 2
  ,height = 20
  ,limits = (0,100)
  ,bgColor = white
  ,minColor = red
  ,maxColor = green
  ,lastColor = blue
  ,minMarker = True
  ,maxMarker = True
  ,lastMarker = True
  }


renderSmooth :: SparkOptions -> [Float] -> IO Image
renderSmooth opt ds = do
  let w = 4 + (step opt) * (length ds - 1)
      h = height opt 
      dmin = fst (limits opt)
      dmax = snd (limits opt)
      coords = zip [1,(1+(step opt))..(1+(step opt)*(length ds))]
                   [h - round( (y-(fi dmin)) / ((fi (dmax-dmin+1)) / (fi (h-4))) )
                        | y <- ds ]
      minpt = maximumBy (comparing snd) coords -- because y increases as we go down
      maxpt = minimumBy (comparing snd) coords
      endpt = last coords

  img <- newImage (w,h)
  drawFilledRectangle (0,0) (w,h) (bgColor opt) img
  zipWithM_ (\p1 p2 -> antiAliased (drawLine p1 p2) grey img)
            coords (drop 1 coords)
  when (minMarker opt) (uncurry drawFilledRectangle (boxpt minpt) (minColor opt) img)
  when (maxMarker opt) (uncurry drawFilledRectangle (boxpt maxpt) (maxColor opt) img)
  when (lastMarker opt) (uncurry drawFilledRectangle (boxpt endpt) (lastColor opt)
                         img)
  return img


renderBar :: SparkOptions -> [Float] -> IO Image
renderBar opt ds = do
  let w = 4 + (step opt) * (length ds - 1) + bw2 * length ds
      h = height opt
      dmin = fst (limits opt)
      dmax = snd (limits opt)
      bw = 1
      bw2 = 2 * bw
      coords = zip [1,(1+(step opt)+bw2)..(1+((step opt)+bw2)*(length ds))]
                   [h - round( (y-(fi dmin)) / ((fi (dmax-dmin+1)) / (fi (h-4))) )
                        | y <- ds ]
      minpt = maximumBy (comparing snd) coords -- because y increases as we go down
      maxpt = minimumBy (comparing snd) coords
      endpt = last coords

  img <- newImage (w,h)
  drawFilledRectangle (0,0) (w,h) (bgColor opt) img
  forM_ coords $ \(x,y) ->
    antiAliased (drawFilledRectangle (x-bw,y) (x+bw,h)) grey img
  when (minMarker opt) (uncurry drawFilledRectangle (boxpt minpt) (minColor opt) img)
  when (maxMarker opt) (uncurry drawFilledRectangle (boxpt maxpt) (maxColor opt) img)
  when (lastMarker opt) (uncurry drawFilledRectangle (boxpt endpt) (lastColor opt)
                         img)
  return img


encodePngAsDataUrl :: Image -> IO String
encodePngAsDataUrl img = savePngByteString img >>= return . encode . unpack

fi :: (Num b, Integral a) => a -> b
fi x = fromIntegral x

boxpt :: (Num a) => (a,a) -> ((a,a),(a,a))
boxpt (x,y) = (,) (x-1,y-1) (x+1,y+1)

white,grey,red,green,blue :: Color
white = rgb 0xff 0xff 0xff
grey = rgb 0x88 0x88 0x88
red = rgb 0xff 0x00 0x00
green = rgb 0x00 0xff 0x00
blue = rgb 0x00 0x00 0xff

