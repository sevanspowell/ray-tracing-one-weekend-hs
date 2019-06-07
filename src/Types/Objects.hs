{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Objects where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)

import Types.Hittable
import Types.Vec3
import Types.Ray

data Sphere
  = Sphere { _sphereC :: Vec3
           , _sphereR :: Float
           }
  deriving (Eq, Show)
makeLenses ''Sphere

data Object = ObjectSphere Sphere

sphereHit :: HitFn Sphere
sphereHit ray tMin tMax sphere = 
  let
    rOrigin      = ray ^. rayOrigin
    rDir         = ray ^. rayDir

    center       = sphere ^. sphereC
    radius       = sphere ^. sphereR

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
        then Just $ HitInfo t p n
        else Nothing

objectHit :: HitFn Object
objectHit ray tMin tMax object =
  case object of
    ObjectSphere sphere -> sphereHit ray tMin tMax sphere

listHit :: HitFn [Object]
listHit ray tMin tMax = foldr hitClosest Nothing
  where
    hitClosest :: Object -> Maybe HitInfo -> Maybe HitInfo
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
  
