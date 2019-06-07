{-# LANGUAGE TemplateHaskell #-}
module Types.Hittable where

import Control.Lens.TH (makeLenses)

import Types.Vec3
import Types.Ray

data HitInfo
  = HitInfo { _hiTime   :: Float
            , _hiPos    :: Vec3
            , _hiNormal :: Vec3
            }
  deriving (Eq, Show)
makeLenses 'HitInfo

type HitFn a
  =  Ray
  -- ^ Check to see if this ray,
  -> Float
  -- ^ between tMin
  -> Float
  -- ^ and tMax,
  -> a
  -- ^ hit this object
  -> Maybe HitInfo
  -- ^ returning this info.
