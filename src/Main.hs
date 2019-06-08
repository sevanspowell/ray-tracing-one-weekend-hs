{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (div)

import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import System.Random (newStdGen, getStdGen, random, StdGen, RandomGen, Random)
import Data.Foldable (for_)
import Data.Function ((&))
import Control.Lens ((^.))
import Control.Monad (foldM)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.State (evalStateT, evalState)
import Control.Monad.IO.Class (liftIO)
import Numeric.Limits (maxValue)

import Types.Colour (Colour, clrR, clrG, clrB, mkColour)
import Types.Ray (Ray, rayDir, rayOrigin, travel, mkRay)
import Types.Vec3 (Vec3(Vec3), scale, add, vec3Y, sub, dot, mkUnit, vec3X, vec3Z, squaredLength)
import Types.Hittable (HitFn, hiNormal, hiPos)
import Types.Objects (sphereHit, Sphere(Sphere), Object(ObjectSphere), listHit)
import Types.Camera (Camera(Camera), getRay)

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

didHitSphere :: Vec3 -> Float -> Ray -> Bool
didHitSphere center radius ray =
  let
    rOrigin      = ray ^. rayOrigin
    rDir         = ray ^. rayDir

    oc           = rOrigin `sub` center
    a            = rDir `dot` rDir
    b            = 2.0 * (oc `dot` rDir)
    c            = (oc `dot` oc) - radius * radius

    discriminant = b*b - 4*a*c
  in
    discriminant > 0

blueSkyWithSphere :: Ray -> Colour
blueSkyWithSphere ray = 
  let
    dir = ray ^. rayDir
    t   = 0.5 * (dir ^. vec3Y + 1.0) 

    white = Vec3 1.0 1.0 1.0
    blue  = Vec3 0.5 0.7 1.0

    sC = Vec3 0.0 0.0 (-1.0)
    sR = 0.5
  in
    if didHitSphere sC sR ray
    then toRGBColour $ Vec3 1.0 0.0 0.0
    else toRGBColour $ simpleLerp white blue t

blueSkyWithSphereNormals :: Ray -> Colour
blueSkyWithSphereNormals ray =
  let
    dir  = ray ^. rayDir
    lerp = 0.5 * (dir ^. vec3Y + 1.0) 

    white = Vec3 1.0 1.0 1.0
    blue  = Vec3 0.5 0.7 1.0

    sC = Vec3 0.0 0.0 (-1.0)
    sR = 0.5
  in
    case hitInfoSphere sC sR ray of
      Nothing -> toRGBColour $ simpleLerp white blue lerp
      Just t  ->
        let
          n = mkUnit $ travel ray t `sub` sC
        in
          toRGBColour $
            Vec3 (n ^. vec3X + 1) (n ^. vec3Y + 1) (n ^. vec3Z + 1)
              `scale` 0.5

blueSkyWithSphereNormals' :: Ray -> Colour
blueSkyWithSphereNormals' ray =
  case sphereHit ray 0.0 maxValue (Sphere (Vec3 0.0 0.0 (-1.0)) 0.5) of
    Nothing ->
      let
        dir  = ray ^. rayDir
        lerp = 0.5 * (dir ^. vec3Y + 1.0) 

        white = Vec3 1.0 1.0 1.0
        blue  = Vec3 0.5 0.7 1.0
      in
        toRGBColour $ simpleLerp white blue lerp
    Just hi ->
      let
        n = hi ^. hiNormal
      in
        toRGBColour $
          Vec3 (n ^. vec3X + 1) (n ^. vec3Y + 1) (n ^. vec3Z + 1)
            `scale` 0.5

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

wonderfulWorld :: HitFn a -> a -> Ray -> Vec3
wonderfulWorld hitFn world ray =
  case hitFn ray 0.0 maxValue world of
    Nothing -> blueSkyColour ray
    Just hi -> normalColour (hi ^. hiNormal)

wonderfulWorldMat :: (RandomGen g, MonadState g m) => HitFn a -> a -> Ray -> m Vec3
wonderfulWorldMat hitFn world ray =
  case hitFn ray 0.001 maxValue world of
    Nothing -> pure $ blueSkyColour ray
    Just hi -> do
      let
        hitPos = hi ^. hiPos
        hitNormal = hi ^. hiNormal

      unitSpherePoint <- randomInUnitSphere
      let
        target = hitPos `add` hitNormal `add` unitSpherePoint

        rayO = hitPos
        rayD = target `sub` hitPos
        newRay = mkRay rayO rayD

      (`scale` 0.5) <$> wonderfulWorldMat hitFn world newRay

nextRandom :: (RandomGen g, MonadState g m, Random a) => m a
nextRandom = do
  g <- get
  let (a, g') = random g
  put g'
  pure a

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

-- TODO Use MonadState and helper function to thread generator
randomInUnitSphere :: (RandomGen g, MonadState g m) => m Vec3
randomInUnitSphere = do
  rX <- nextRandom
  rY <- nextRandom
  rZ <- nextRandom

  let p = ((Vec3 rX rY rZ) `scale` 2.0) `sub` (Vec3 1 1 1)

  if squaredLength p >= 1.0
  then randomInUnitSphere
  else pure p

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
      world = [ ObjectSphere $ Sphere (Vec3 0.0 0.0 (-1.0)) 0.5
              , ObjectSphere $ Sphere (Vec3 0.0 (-100.5) (-1.0)) 100
              ]

      colourFn :: (RandomGen g, MonadState g m) => Ray -> m Vec3
      colourFn = wonderfulWorldMat listHit world

      numSamples = 100

    for_ [numRows-1,numRows-2..0] $ \y ->
      for_ [0..numCols-1] $ \x -> do
        g <- newStdGen

        let
          sampleAcc :: (RandomGen g, MonadState g m) => Vec3 -> Int -> m Vec3
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
