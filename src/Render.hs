module Render
where

import Data.Array
import Array2D
import Map


instance Render Pixel where
instance Render Char where


data Pixel = Pixel Char

type Screen = Map Pixel

class PixelType a where
  pixel :: a -> Pixel

instance PixelType Pixel where
  pixel p = p

instance PixelType Char where
  pixel c = Pixel c

instance Show Pixel where
  show (Pixel c) = [c]

class (PixelType a) => Render a where
  makeScreen :: Array2D a -> Screen
  makeScreen dst = Map . Array2D $(array (bounds.arr $dst) pixs)
    where
      pixs = [(i, pixel a) | (i,a) <- (assocs.arr $dst)]

  render :: Screen -> (Array2D a) -> Screen
  -- renders an Array2D of Show on an Array2D of Chars
  render dst src = Map . Array2D $(accum (\d s -> s) (arr.arr2D $dst) srcs)
    where
      srcs = [(i, pixel a) | (i,a) <- assocs.arr $src]

  renderAt :: Screen -> (Array2D a) -> IntVec2 -> Screen
  -- renders an Array2D of PixelType on an Array2D of Chars
  renderAt dst src (px, py) = render dst updated
    where
      (b0, b1) = bounds.arr $src
      transform (x,y) = (x+px,y+py)
      offset = (transform b0,transform b1)
      updated = Array2D (ixmap offset transform (arr src))
