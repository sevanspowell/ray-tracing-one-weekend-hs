
module Types.Colour (Colour, clrR, clrG, clrB, mkColour) where

import Data.Function ((&))
import Control.Lens (lens, (^.), (.~), Lens')

import Types.Vec3 (Vec3(Vec3), vec3X, vec3Y, vec3Z)

newtype Colour = Colour Vec3

mkColour :: Int -> Int -> Int -> Colour
mkColour x y z = Colour $ Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)

clrR :: Lens' Colour Int
clrR =
  lens
    (\(Colour v) -> truncate $ v ^. vec3X)
    (\(Colour v) r -> Colour $ v & vec3X .~ (fromIntegral r))

clrG :: Lens' Colour Int
clrG =
  lens
  (\(Colour v) -> truncate $ v ^. vec3Y)
  (\(Colour v) g -> Colour $ v & vec3Y .~ (fromIntegral g))

clrB :: Lens' Colour Int
clrB =
  lens
  (\(Colour v) -> truncate $ v ^. vec3Z)
  (\(Colour v) b -> Colour $ v & vec3Z .~ (fromIntegral b))
