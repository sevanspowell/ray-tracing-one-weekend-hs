{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import System.IO
import Data.Traversable
import Data.Foldable
import Control.Lens

import Types.Colour

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype RelativeCoordinate = RelativeCoordinate Float
  deriving (RealFrac, Num, Fractional, Real, Ord, Eq, Show)

mkRelativeCoordinate :: Float -> Maybe RelativeCoordinate
mkRelativeCoordinate x =
  if x >= 0.0 && x <= 1.0
  then Just $ RelativeCoordinate x
  else Nothing

toPPMTriplet :: Colour -> String
toPPMTriplet c = (show $ c ^. clrR) <> " " <> (show $ c ^. clrG) <> " " <> (show $ c ^. clrB)

image :: RelativeCoordinate -> RelativeCoordinate -> Colour
image relX relY = mkColour (truncate $ relX * 255.99) (truncate $ relY * 255.99) (truncate $ 0.2 * 255.99)

-- data ImageFormat = ImageFormatPPM

-- imageWriter :: Int -> Int -> ImageFormat -> ConduitT Colour () IO ()
-- imageWriter numRows numCols

simpleImageWrite numRows numCols = do
  withFile "test.ppm" WriteMode $ \h -> do
    hPutStrLn h $ "P3"
    hPutStrLn h $ show numCols <> " " <> show numRows
    hPutStrLn h $ "255"

    for_ [numRows-1,numRows-2..0] $ \y ->
      for_ [0..numCols-1] $ \x -> do
        let col = image (fromIntegral x / fromIntegral numCols) (fromIntegral y / fromIntegral numRows)
        hPutStrLn h $ toPPMTriplet $ col

-- TODO image coordinate space is a little implicit, should we convert to standard image coordinate space?
