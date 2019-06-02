{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import System.IO
import Data.Traversable
import Data.Foldable

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype RelativeCoordinate = RelativeCoordinate Float
  deriving (RealFrac, Num, Fractional, Real, Ord, Eq, Show)

mkRelativeCoordinate :: Float -> Maybe RelativeCoordinate
mkRelativeCoordinate x =
  if x >= 0.0 && x <= 1.0
  then Just $ RelativeCoordinate x
  else Nothing

data Colour = RGBColour Int Int Int
  deriving Show

mkRGBColour :: Int -> Int -> Int -> Colour
mkRGBColour r g b = RGBColour r g b

toPPMTriplet :: Colour -> String
toPPMTriplet (RGBColour r g b) = show r <> " " <> show g <> " " <> show b

image :: RelativeCoordinate -> RelativeCoordinate -> Colour
image relX relY = RGBColour (truncate $ relX * 255.99) (truncate $ relY * 255.99) (truncate $ 0.2 * 255.99)

data ImageFormat = ImageFormatPPM

-- imageWriter :: Int -> Int -> ImageFormat -> ConduitT Colour () IO ()
-- imageWriter numRows numCols

simpleImageWrite numRows numCols = do
  withFile "test.ppm" WriteMode $ \h -> do
    hPutStrLn h $ "P3"
    hPutStrLn h $ show numCols <> " " <> show numRows
    hPutStrLn h $ "255"

    for_ [numRows-1,numRows-2..0] $ \y ->
      for_ [0..numCols-1] $ \x -> do
        hPutStrLn h $ toPPMTriplet $ image (fromIntegral x / fromIntegral numCols) (fromIntegral y / fromIntegral numRows)

-- TODO image coordinate space is a little implicit, should we convert to standard image coordinate space?
