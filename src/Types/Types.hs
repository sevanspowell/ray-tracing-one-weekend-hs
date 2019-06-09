{-# LANGUAGE TemplateHaskell #-}

module Types.Types where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.State.Class (MonadState)
import System.Random (RandomGen)
import Types.Random (randomInUnitSphere)
import Types.Ray (Ray, mkRay, rayDir)
import Types.Vec3 (Vec3, add, sub, reflect)

data ScatteredRay
  = ScatteredRay { _scatAttenuation  :: Vec3
                 , _scatRay :: Ray
                 }
  deriving (Eq, Show)
makeLenses 'ScatteredRay

type Material m = Ray -> HitInfo m -> m (Maybe ScatteredRay)

data HitInfo m
  = HitInfo { _hiTime     :: Float
            , _hiPos      :: Vec3
            , _hiNormal   :: Vec3
            , _hiMaterial :: Material m
            }
makeLenses 'HitInfo

type HitFn m a
  =  Ray
  -- ^ Check to see if this ray,
  -> Float
  -- ^ between tMin
  -> Float
  -- ^ and tMax,
  -> a
  -- ^ hit this object
  -> Maybe (HitInfo m)
  -- ^ returning this info.

lambertian
  :: ( RandomGen g
     , MonadState g m
     )
  => Vec3
  -> Material m
lambertian albedo _ hi = do
  unitSpherePoint <- randomInUnitSphere

  let
    hitPos    = hi ^. hiPos
    hitNormal = hi ^. hiNormal
    target    = hitPos `add` hitNormal `add` unitSpherePoint

    rayO   = hitPos
    rayD   = target `sub` hitPos
    scattered = mkRay rayO rayD

  pure . Just $ ScatteredRay albedo scattered

metal :: Applicative m => Vec3 -> Material m 
metal albedo rayIn hi =
  let
    reflected = reflect (rayIn ^. rayDir) (hi ^. hiNormal)
    scattered = mkRay (hi ^. hiPos) reflected
    attenuation = albedo
  in
    pure . Just $ ScatteredRay attenuation scattered

-- dielectrics
--   :: Float
--   -- ^ Reflective index
--   -> Material m
-- dielectrics reflIndex rayIn hi =
