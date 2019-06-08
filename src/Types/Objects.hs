{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Types.Objects where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)

import Types.Types (HitFn, HitInfo(HitInfo), hiTime, Material)
import Types.Vec3 (Vec3, sub, dot, scale)
import Types.Ray (rayOrigin, rayDir, travel)

data Sphere m
  = Sphere { _sphereC   :: Vec3
           , _sphereR   :: Float
           , _sphereMat :: Material m
           }
makeLenses ''Sphere

data Object m = ObjectSphere (Sphere m)

sphereHit :: HitFn m (Sphere m)
sphereHit ray tMin tMax sphere = 
  let
    rOrigin      = ray ^. rayOrigin
    rDir         = ray ^. rayDir

    center       = sphere ^. sphereC
    radius       = sphere ^. sphereR
    material     = sphere ^. sphereMat

    oc           = rOrigin `sub` center
    a            = rDir `dot` rDir
    b            = 2.0 * (oc `dot` rDir)
    c            = (oc `dot` oc) - radius * radius

    discriminant = b*b - 4*a*c
  in
    if discriminant < 0
    then Nothing
    else
      let
        t = ((-b) - sqrt(discriminant)) / (2.0 * a)
        p = ray `travel` t
        n = (p `sub` center) `scale` (1.0 / radius)
      in
        if t < tMax && t > tMin
        then Just $ HitInfo t p n material
        else Nothing

objectHit :: HitFn m (Object m)
objectHit ray tMin tMax object =
  case object of
    ObjectSphere sphere -> sphereHit ray tMin tMax sphere

listHit :: HitFn m [Object m]
listHit ray tMin tMax = foldr hitClosest Nothing
  where
    -- hitClosest :: Object -> Maybe (HitInfo m) -> Maybe (HitInfo m)
    -- We've hit nothing yet
    hitClosest o Nothing = 
      case objectHit ray tMin tMax o of
        Nothing -> Nothing
        Just hi -> Just hi
    -- We've hit something before
    hitClosest o (Just hi) =
      case objectHit ray tMin (hi ^. hiTime) o of
        Nothing  -> Just hi
        Just hi' -> Just hi'
  
