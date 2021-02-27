module HyperSplit.Algorithms where

import qualified Graphics.Image as I
import System.Random
import Data.List
import HyperSplit.Utils

type Algorithm = StdGen -> Picture -> Picture

nothing :: Algorithm
nothing = flip const

-----proc :: (Picture -> Picture) -> String -> IO Picture
-----proc fn infile = I.readImageRGB I.VU infile >>= return . fn
-----
-----run :: (Picture -> Picture) -> String -> IO ()
-----run fn infile = do
-----        img <- I.readImageRGB I.VU infile
-----        I.displayImage (fn img)

-----runWithRandom :: (StdGen -> Picture -> Picture) -> String -> IO ()
-----runWithRandom fn infile = do
-----        g <- getStdGen
-----        run (fn g) infile

bleed :: Algorithm
bleed g = applyRowFnsToImage (map repeatElemsSafe rands2d)
  where rands2d = map (map randfn) $ map (randomRs (1, 20)) (infinisplit g)
        randfn n = if n > 19 then n * 4 else div n 2

bleedRgb :: Algorithm
bleedRgb g = applyRgb (bleed g1, bleed g2, bleed g3)
  where (g1, g2, g3) = split3 g

fullPixelSort :: Algorithm
fullPixelSort g = applyRowFnsToImage rowFns
  where rowFns = repeat $ concatMap (sortBy sortFn) . cluster 2 brightness
        sortFn = \a b -> compare (brightness a) (brightness b)

cluster :: Double -> (a -> Double) -> [a] -> [[a]]
cluster bins f pixels = splitWith (\last current -> seg last == seg current) pixels
  where max_br = maximum $ map f pixels
        min_br = minimum $ map f pixels
        range = max_br - min_br
        seg p = floor $ (bins *) $ ((f p) - min_br) / range

splitWith :: (a -> a -> Bool) -> [a] -> [[a]]
splitWith p xs =
    case span2 p xs of
        ([], []) -> []
        (xs, []) -> [xs]
        (xs, ys) -> xs : splitWith p ys

span2 :: (a -> a -> Bool) -> [a] -> ([a], [a])
span2 _ [] = ([], [])
span2 p (x:xs) = (reverse a, b)
  where (a, b) = span2Helper p [x] xs

span2Helper :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a])
span2Helper p acc [] = (acc, [])
span2Helper p (acc:accs) (alpha:betas) =
  if p acc alpha then
    span2Helper p (alpha:acc:accs) betas
  else
    ((acc:accs), (alpha:betas))

vFullPixelSort :: Algorithm
vFullPixelSort = verticalize . fullPixelSort

pixelSort :: Algorithm
pixelSort g = applyRowFnsToImage rowFns
  where rowFns = map (\gen -> roughSortBy gen (40, 56) sortFn) (infinisplit g)
        sortFn = \a b -> compare (brightness a) (brightness b)

asdfPixelSort :: (Picxel -> Bool) -> Picture -> Picture
asdfPixelSort break = applyRowFnsToImage (repeat (asdfRowSort break))

asdfPixelSortRgb :: (Picxel -> Bool) -> (Picxel -> Bool) -> (Picxel -> Bool) -> Picture -> Picture
asdfPixelSortRgb fn1 fn2 fn3 = applyRgb (red, green, blue)
  where red = asdfPixelSort fn1
        green = asdfPixelSort fn2
        blue = asdfPixelSort fn3

vBleedRgb :: Algorithm
vBleedRgb = verticalize . bleedRgb
vPixelSort :: Algorithm
vPixelSort = verticalize . pixelSort
--vAsdfPixelSortRgb :: Algorithm
--vAsdfPixelSortRgb = verticalize . asdfPixelSortRgb
--vAsdfPixelSort :: Algorithm
--vAsdfPixelSort = verticalize . asdfPixelSort

--hyperSplit g = map (drop Green . (applyRgb (bleed g1, toBlack, bleed g3)))
--  where (g1, g2, g3) = split3 g

shift :: Algorithm
shift g = applyRowFnsToImage (repeatElems (randomRs (1, 10) g1) rowFns)
  where (g1, g2, g3) = split3 g
        rowFns :: [[Picxel] -> [Picxel]]
        rowFns = zipWith fn (randomRs (-360, 360) g2) (randoms g3)
        fn :: Int -> ColorComponent -> ([Picxel] -> [Picxel])
        fn a b = (rotate a) . map (keep b)

rgbShiftElm :: Int -> Algorithm
rgbShiftElm a g img = applyRgb (red, green, blue) img
  where (g1, g2, g3) = split3 g
        red :: Picture -> Picture
        red = \img -> fst $ shiftElm a (img, g1)
        green = id -- \img -> fst $ shiftElm a (img, g2)
        blue = id -- \img -> fst $ shiftElm a (img, g3)

shiftElm :: Int -> (Picture, StdGen) -> (Picture, StdGen)
shiftElm amount (img, s) =
    let
        (s1, s2, s3) = split3 s
        randomDegrees = randomRs (-amount, amount) s1
        draggedRowFns = repeatElems (randomRs (10,30) s2) rowFns
        rowFns = map rotate randomDegrees
    in
        (applyRowFnsToImage draggedRowFns img, s3)
