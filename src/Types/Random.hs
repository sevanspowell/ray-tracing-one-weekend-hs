{-# LANGUAGE TemplateHaskell #-}

module Types.Random where

import System.Random (RandomGen, Random, random)
import Control.Monad.State.Class (MonadState, get, put)

import Types.Vec3 (Vec3(Vec3), scale, sub, squaredLength)

nextRandom :: (RandomGen g, MonadState g m, Random a) => m a
nextRandom = do
  g <- get
  let (a, g') = random g
  put g'
  pure a

randomInUnitSphere :: (RandomGen g, MonadState g m) => m Vec3
randomInUnitSphere = do
  rX <- nextRandom
  rY <- nextRandom
  rZ <- nextRandom

  let p = ((Vec3 rX rY rZ) `scale` 2.0) `sub` (Vec3 1 1 1)

  if squaredLength p >= 1.0
  then randomInUnitSphere
  else pure p
