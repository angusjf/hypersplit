module Utils where

import System.Random
import Data.List
import qualified Graphics.Image as I

data ColorComponent = Red | Blue | Green

type Picture = I.Image I.VU I.RGB Double
type Picxel = I.Pixel I.RGB Double

brightness :: Picxel -> Double
brightness (I.PixelRGB r g b) = r + g + b

rgb :: Picture -> (Picture, Picture, Picture)
rgb img = (red, green, blue)
  where red   = I.map (keep Red)   img
        green = I.map (keep Green) img
        blue  = I.map (keep Blue)  img

applyRgb :: (Picture -> Picture, Picture -> Picture, Picture -> Picture)
            -> Picture -> Picture
applyRgb (f1, f2, f3) img = f1 red + f2 green + f3 blue
  where (red, green, blue) = rgb img

keep :: ColorComponent -> Picxel -> Picxel
keep Red   (I.PixelRGB r g b) = I.PixelRGB r 0 0
keep Green (I.PixelRGB r g b) = I.PixelRGB 0 g 0
keep Blue  (I.PixelRGB r g b) = I.PixelRGB 0 0 b

split3 :: StdGen -> (StdGen, StdGen, StdGen)
split3 g = (g1, g2, g3)
  where (g1, rest) = split g
        (g2, g3)   = split rest

infinisplit :: StdGen -> [StdGen]
infinisplit g = g1:infinisplit g2
  where (g1, g2) = split g

repeatElems :: [Int] -> [a] -> [a]
repeatElems (n:ns) all@(x:_) = repeats ++ repeatElems ns rest
  where repeats = take (length all) $ replicate n x
        rest    = drop n all
repeatElems _ xs = xs

toBlack :: Picture -> Picture
toBlack = I.map (const black)
  where black :: Picxel
        black = I.PixelRGB 0 0 0

applyRowFnsToImage :: [[Picxel] -> [Picxel]] -> Picture -> Picture
applyRowFnsToImage fns = I.fromLists . zipWith ($) fns . I.toLists

roughSortBy :: StdGen -> (Int, Int) -> (a -> a -> Ordering) -> [a] -> [a]
roughSortBy _ _ _ [] = []
roughSortBy g range fn list = sortBy fn start ++ roughSortBy g' range fn end
  where (n, g') = randomR range g
        start = take n list
        end   = drop n list

verticalize :: (Picture -> Picture) -> Picture -> Picture
verticalize fn = I.transpose . fn . I.transpose

asdfRowSort :: [Picxel] -> [Picxel]
asdfRowSort = concat . map (sortBy sortFn) . breakUp isDark
  where sortFn a b = compare (brightness a) (brightness b)

breakUp :: (a -> Bool) -> [a] -> [[a]]
breakUp fn xs = case end of
  []   -> start : []
  x:xs -> start : [x] : breakUp fn xs
  where (start, end) = break fn xs

isDark :: Picxel -> Bool
isDark = (< 0.5) . brightness
