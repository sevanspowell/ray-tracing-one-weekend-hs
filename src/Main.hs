{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (div)

import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import System.Random (RandomGen, newStdGen)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Maybe (isJust)
import Control.Lens ((^.))
import Control.Monad (foldM)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State (evalStateT, evalState)
import Control.Monad.IO.Class (liftIO)
import Numeric.Limits (maxValue)

import Types.Colour (Colour, clrR, clrG, clrB, mkColour)
import Types.Ray (Ray, rayDir, rayOrigin, travel, mkRay)
import Types.Vec3 (Vec3(Vec3), scale, add, vec3Y, sub, dot, mkUnit, vec3X, vec3Z, mul)
import Types.Types (HitFn, hiNormal, hiPos, lambertian, hiMaterial, metal, scatAttenuation, scatRay, dielectric)
import Types.Objects (sphereHit, Sphere(Sphere), Object(ObjectSphere), listHit)
import Types.Camera (Camera(Camera), getRay)
import Types.Random (randomInUnitSphere, nextRandom)

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

blueSkyColour :: Ray -> Vec3 
blueSkyColour ray =
  let
    dir  = ray ^. rayDir
    lerp = 0.5 * (dir ^. vec3Y + 1.0) 
  
    white = Vec3 1.0 1.0 1.0
    blue  = Vec3 0.5 0.7 1.0
  in
    simpleLerp white blue lerp

normalColour :: Vec3 -> Vec3
normalColour n = 
  Vec3 (n ^. vec3X + 1) (n ^. vec3Y + 1) (n ^. vec3Z + 1)
    `scale` 0.5

withMaterials :: (RandomGen g, MonadState g m) => HitFn m a -> a -> Ray -> m Vec3
withMaterials = go 0
  where
    go :: (RandomGen g, MonadState g m) => Int -> HitFn m a -> a -> Ray -> m Vec3
    go depth hitFn world ray =
      case hitFn ray 0.001 maxValue world of
        Nothing -> pure $ blueSkyColour ray
        Just hi -> do
          let material = hi ^. hiMaterial

          mScattered <- material ray hi

          case mScattered of
            Just scattered
              | depth < 50 -> do
              colour <- go (depth + 1) hitFn world (scattered ^. scatRay)
              pure $
                (scattered ^. scatAttenuation) `mul` colour
            otherwise ->
              pure $ Vec3 0 0 0
            
          -- let
          --   hi pf

testNextRandom :: IO ()
testNextRandom = do
  g <- newStdGen
  (flip evalStateT) g $ do
    (r1 :: Double) <- nextRandom
    (r2 :: Double) <- nextRandom
    (r3 :: Double) <- nextRandom
    liftIO $ do
      print r1
      print r2
      print r3
    
hitInfoSphere :: Vec3 -> Float -> Ray -> Maybe Float
hitInfoSphere center radius ray = 
  let
    rOrigin      = ray ^. rayOrigin
    rDir         = ray ^. rayDir

    oc           = rOrigin `sub` center
    a            = rDir `dot` rDir
    b            = 2.0 * (oc `dot` rDir)
    c            = (oc `dot` oc) - radius * radius

    discriminant = b*b - 4*a*c
  in
    if discriminant < 0
    then Nothing
    else Just $ ((-b) - sqrt(discriminant)) / (2.0 * a)

-- TODO tool for visualizing the output of 100 calls of some of these functions

testR :: IO ()
testR = do
  g <- newStdGen
  flip evalStateT g $ do
    v1 <- randomInUnitSphere
    v2 <- randomInUnitSphere
    v3 <- randomInUnitSphere
    liftIO $ do
      print v1
      print v2
      print v3

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
      origin          = Vec3 0.0 0.0 0.0

      cam = Camera origin lowerLeftCorner horizontal vertical
      world = [ ObjectSphere $ Sphere (Vec3 0.0 0.0 (-1.0)) 0.5 (lambertian $ Vec3 0.1 0.2 0.5)
              , ObjectSphere $ Sphere (Vec3 0.0 (-100.5) (-1.0)) 100 (lambertian $ Vec3 0.8 0.8 0)
              , ObjectSphere $ Sphere (Vec3 1 0 (-1)) 0.5 (metal $ Vec3 0.8 0.6 0.2)
              , ObjectSphere $ Sphere (Vec3 (-1) 0 (-1)) 0.5 (dielectric 1.5)
              ]

      -- colourFn :: (RandomGen g, MonadState g m) => Ray -> m Vec3
      colourFn = withMaterials listHit world

      numSamples = 100

    for_ [numRows-1,numRows-2..0] $ \y ->
      for_ [0..numCols-1] $ \x -> do
        g <- newStdGen

        let
          -- sampleAcc :: (RandomGen g, MonadState g m) => Vec3 -> Int -> m Vec3
          sampleAcc acc _ = do
            (uRand :: Float) <- nextRandom
            (vRand :: Float) <- nextRandom

            let
              u = ((fromIntegral x) + uRand) / (fromIntegral numCols)
              v = ((fromIntegral y) + vRand) / (fromIntegral numRows)
              ray = getRay cam u v

            (acc `add`) <$> colourFn ray

          gammaCorrect c = Vec3 (sqrt(c ^. vec3X)) (sqrt(c ^. vec3Y)) (sqrt(c ^. vec3Z))

          col :: Vec3
          col = foldM sampleAcc (Vec3 0 0 0) [0..numSamples-1]
                & flip evalState g 
                & (`scale` (1 / (fromIntegral numSamples)))
                & gammaCorrect


        hPutStrLn h . toPPMTriplet . toRGBColour $ col
