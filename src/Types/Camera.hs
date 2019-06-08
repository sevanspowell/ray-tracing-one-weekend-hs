{-# LANGUAGE TemplateHaskell #-}

module Types.Camera where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)

import Types.Vec3 (Vec3, add, scale, sub)
import Types.Ray (Ray, mkRay)

data Camera = Camera { _camOrigin     :: Vec3
                     , _camLowerLeft  :: Vec3
                     , _camHorizontal :: Vec3
                     , _camVertical   :: Vec3
                     }
  deriving (Eq, Show)
makeLenses 'Camera

getRay :: Camera -> Float -> Float -> Ray
getRay cam u v =
  let
    origin     = cam ^. camOrigin
    lowerLeft  = cam ^. camLowerLeft
    horizontal = cam ^. camHorizontal
    vertical   = cam ^. camVertical
    dir        = lowerLeft `add` (horizontal `scale` u) `add` (vertical `scale` v) `sub` origin
  in
    mkRay origin dir
