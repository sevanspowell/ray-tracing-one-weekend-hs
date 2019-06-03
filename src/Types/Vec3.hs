{-# LANGUAGE TemplateHaskell #-}

module Types.Vec3 (Vec3(Vec3), vec3X, vec3Y, vec3Z) where

import Prelude (Float, Show, Eq, (+), (-), (*), (/), sqrt, negate, (.), (^))
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
squaredLength (Vec3 x y z) = x^2 + y^2 + z^2

mkUnit :: Vec3 -> Vec3
mkUnit v = v `scale` (1 / length v)
