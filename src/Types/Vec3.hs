{-# LANGUAGE TemplateHaskell #-}

module Types.Vec3 ( Vec3(Vec3)
                  , vec3X
                  , vec3Y
                  , vec3Z
                  , invert
                  , add
                  , sub
                  , mul
                  , div
                  , scale
                  , length
                  , squaredLength
                  , mkUnit
                  , dot
                  , reflect
                  , refract
                  ) where

import Prelude (Float, Show, Eq, (+), (-), (*), (/), sqrt, negate, (.), Maybe(Just, Nothing), (>), ($))
import Control.Lens.TH (makeLenses)

data Vec3 = Vec3 { _vec3X :: Float
                 , _vec3Y :: Float
                 , _vec3Z :: Float
                 }
  deriving (Show, Eq)
makeLenses ''Vec3

invert :: Vec3 -> Vec3
invert (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

mul :: Vec3 -> Vec3 -> Vec3
mul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

div :: Vec3 -> Vec3 -> Vec3
div (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 / x2) (y1 / y2) (z1 / z2)

scale :: Vec3 -> Float -> Vec3
scale (Vec3 x y z) s = Vec3 (x * s) (y * s) (z * s)

length :: Vec3 -> Float
length = sqrt . squaredLength

squaredLength :: Vec3 -> Float
squaredLength (Vec3 x y z) = x*x + y*y + z*z

mkUnit :: Vec3 -> Vec3
mkUnit v = v `scale` (1 / length v)

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

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
