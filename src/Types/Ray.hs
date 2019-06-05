{-# LANGUAGE TemplateHaskell #-}
module Types.Ray where

import Control.Lens.TH (makeLenses)

import Types.Vec3

-- Ray = origin + t*dir
data Ray = Ray { _rayOrigin :: Vec3
               , _rayDir    :: Vec3
               }
  deriving (Eq, Show)
makeLenses ''Ray

-- Make a ray from origin and direction vectors, the direction vector
-- will be made into a unit vector.
mkRay :: Vec3 -> Vec3 -> Ray
mkRay origin dir = Ray origin (mkUnit dir)

-- Start at the origin of the ray and travel 't' in the direction of
-- the ray and return the point we reach.
travel :: Ray -> Float -> Vec3
travel (Ray origin dir) t = origin `add` (dir `scale` t)
