{-# LANGUAGE TemplateHaskell #-}

module Types.Types where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.State.Class (MonadState)
import System.Random (RandomGen)
import Types.Random (randomInUnitSphere)
import Types.Ray (Ray, mkRay, rayDir)
import Types.Vec3 (Vec3(Vec3), add, sub, dot, scale, mkUnit)

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

reflect
  :: Vec3
  -- ^ Reflect this ray
  -> Vec3
  -- ^ around this normal
  -> Vec3
reflect v n = v `sub` (n `scale` (2 * dot v n))

refract
  :: Vec3
  -- ^ Reflect this ray
  -> Vec3
  -- ^ around this normal
  -> Float
  -- ^ refractive indices ratio: ni/nt
  -> Maybe Vec3
  -- ^ return refracted ray or, if reflected, return nothing.
refract v n niOverNt =
  let
    uv = mkUnit v
    dt = dot uv n
    discriminant = 1.0 - niOverNt*niOverNt*(1-dt*dt)
  in
    if discriminant > 0
    then Just $ ((uv `sub` (n `scale` dt)) `scale` niOverNt) `sub` (n `scale` sqrt(discriminant))
    else Nothing

schlick :: Float -> Float -> Float
schlick cosine refIdx =
  let
    r0 = (1 - refIdx) / (1 + refIdx)
    r0Squared = r0 * r0
  in
    r0Squared + (1 - r0Squared) * ((1 - cosine) ^ 5)

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

dielectric
  :: Applicative m
  => Float
  -- ^ Reflective index
  -> Material m
dielectric reflIndex rayIn hi =
  let
    rayInDir                  = rayIn ^. rayDir
    surfaceNormal             = hi ^. hiNormal
    -- reflected                 = reflect rayInDir surfaceNormal
    attenuation               = Vec3 1 1 1
    (outwardNormal, niOverNt) =
      if (dot rayInDir surfaceNormal) > 0
      then (surfaceNormal `scale` (-1), reflIndex)
      else (surfaceNormal, 1.0 / reflIndex)
  in
    case refract rayInDir outwardNormal niOverNt of
      Just refracted -> pure . Just $ ScatteredRay attenuation (mkRay (hi ^. hiPos) refracted)
      Nothing        -> pure Nothing
