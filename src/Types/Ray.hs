{-# LANGUAGE TemplateHaskell #-}
module Types.Ray where

import Control.Lens.TH (makeLenses)

import Types.Vec3

-- Ray = origin + t*dir
data Ray = Ray { _rayOrigin :: Vec3
               , _rayDir    :: Vec3
               }
makeLenses ''Ray

-- Start at the origin of the ray and travel 't' in the direction of
-- the ray and return the point we reach.
travel :: Ray -> Float -> Vec3
travel (Ray origin dir) t = origin `add` (dir `scale` t)
