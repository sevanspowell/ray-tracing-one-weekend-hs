{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import System.IO
import Data.Foldable
import Control.Lens

import Types.Colour
import Types.Ray
import Types.Vec3

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

testImage :: RelativeCoordinate -> RelativeCoordinate -> Colour
testImage relX relY = mkColour (truncate $ relX * 255.99) (truncate $ relY * 255.99) (truncate $ (0.2 * 255.99 :: Float))

-- data ImageFormat = ImageFormatPPM

-- imageWriter :: Int -> Int -> ImageFormat -> ConduitT Colour () IO ()
-- imageWriter numRows numCols

-- Float should be in range [0,1]
simpleLerp :: Vec3 -> Vec3 -> Float -> Vec3
simpleLerp v1 v2 t = (v1 `scale` (1.0 - t)) `add` (v2 `scale` t)

toRGBColour :: Vec3 -> Colour
toRGBColour (Vec3 r g b) = mkColour (toColourChannel r) (toColourChannel g) (toColourChannel b)

toColourChannel :: Float -> Int
toColourChannel x = truncate $ x * 255.99

blueSky :: Ray -> Colour
blueSky ray =
  let
    dir = ray ^. rayDir
    -- Convert y-component of ray direction (-1.0 <= y <= 1.0)
    -- to (0.0 <= y <= 2.0) then (0.0 <= y <= 1.0).
    t   = 0.5 * (dir ^. vec3Y + 1.0) 

    white = Vec3 1.0 1.0 1.0
    blue  = Vec3 0.5 0.7 1.0
  in
    -- At 0.0: white, at 1.0: blue, lerp in-between
    toRGBColour $ simpleLerp white blue t
  

simpleImageWrite :: Int -> Int -> IO ()
simpleImageWrite numRows numCols = do
  withFile "test.ppm" WriteMode $ \h -> do
    hPutStrLn h $ "P3"
    hPutStrLn h $ show numCols <> " " <> show numRows
    hPutStrLn h $ "255"

    let
      lowerLeftCorner = Vec3 (-2.0) (-1.0) (-1.0)
      horizontal      = Vec3 4.0 0.0 0.0
      vertical        = Vec3 0.0 2.0 0.0
      origin          = Vec3 0.0 2.0 0.0

    for_ [numRows-1,numRows-2..0] $ \y ->
      for_ [0..numCols-1] $ \x -> do

        let
          u = (fromIntegral x) / fromIntegral numCols
          v = (fromIntegral y) / fromIntegral numRows
        
          ray = mkRay origin (lowerLeftCorner `add` (horizontal `scale` u) `add` (vertical `scale` v))

        -- let col = testImage (fromIntegral x / fromIntegral numCols) (fromIntegral y / fromIntegral numRows)
        let col = blueSky ray

        hPutStrLn h $ toPPMTriplet $ col
