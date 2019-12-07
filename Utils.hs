module Utils where

import System.Random
import Data.List
import Data.Tuple
import qualified Graphics.Image as I

data ColorComponent = Red | Green | Blue deriving (Show, Eq)

instance Random ColorComponent where
  randomR (lo, hi) g = let (n, g') = randomR (colorToInt lo, colorToInt hi) g in (intToColor n, g')
    where intToColor :: Int -> ColorComponent
          intToColor 0 = Red
          intToColor 1 = Green
          intToColor 2 = Blue
          colorToInt :: ColorComponent -> Int
          colorToInt Red = 0
          colorToInt Green = 1
          colorToInt Blue = 2

  random g = randomR (Red, Blue) g

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

safeRepeatElems :: [Int] -> [a] -> [a]
safeRepeatElems = undefined

repeatElems :: [Int] -> [a] -> [a]
repeatElems (n:ns) all@(x:_) = repeats ++ repeatElems ns rest
  where repeats = replicate n x
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

asdfRowSort :: (Picxel -> Bool) -> [Picxel] -> [Picxel]
asdfRowSort break = concatMap (sortBy sortFn) . breakUp break
  where sortFn a b = compare (brightness a) (brightness b)

breakUp :: (a -> Bool) -> [a] -> [[a]]
breakUp fn = swapApply (span fn, break fn)

swapApply (f, g) xs = case f xs of 
    ([], end)    -> swapApply (g, f) end
    (start, [])  -> start : []
    (start, end) -> start : swapApply (g, f) end

luminosity :: Picxel -> Double
luminosity (I.PixelRGB r g b) = (maximum [r, g, b] + minimum [r, g, b]) / 2

saturation :: Picxel -> Double
saturation (I.PixelRGB r g b) | l < 1     = (maximum [r, g, b] - minimum [r, g, b]) / (1 - abs (2 * l - 1))
                              | otherwise = 0
  where l = luminosity (I.PixelRGB r g b)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n $ cycle xs
