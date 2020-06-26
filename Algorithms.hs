module Algorithms where

import qualified Graphics.Image as I
import System.Random
import Data.List
import Utils

type Algorithm = StdGen -> Picture -> Picture

proc :: (Picture -> Picture) -> String -> IO Picture
proc fn infile = I.readImageRGB I.VU infile >>= return . fn

run :: (Picture -> Picture) -> String -> IO ()
run fn infile = do
        img <- I.readImageRGB I.VU infile
        I.displayImage (fn img)

runWithRandom :: (StdGen -> Picture -> Picture) -> String -> IO ()
runWithRandom fn infile = do
        g <- getStdGen
        run (fn g) infile

bleed :: StdGen -> Picture -> Picture
bleed g = applyRowFnsToImage (map repeatElemsSafe rands2d)
  where rands2d = map (map randfn) $ map (randomRs (1, 20)) (infinisplit g)
        randfn n = if n > 19 then n * 4 else div n 2

bleedRgb :: StdGen -> Picture -> Picture
bleedRgb g = applyRgb (bleed g1, bleed g2, bleed g3)
  where (g1, g2, g3) = split3 g

pixelSort :: StdGen -> Picture -> Picture
pixelSort g = applyRowFnsToImage rowFns
  where rowFns = map (\gen -> roughSortBy gen (4, 36) sortFn) (infinisplit g)
        sortFn = \a b -> compare (brightness a) (brightness b)

asdfPixelSort :: (Picxel -> Bool) -> Picture -> Picture
asdfPixelSort break = applyRowFnsToImage (repeat (asdfRowSort break))

asdfPixelSortRgb :: (Picxel -> Bool) -> (Picxel -> Bool) -> (Picxel -> Bool) -> Picture -> Picture
asdfPixelSortRgb fn1 fn2 fn3 = applyRgb (red, green, blue)
  where red = asdfPixelSort fn1
        green = asdfPixelSort fn2
        blue = asdfPixelSort fn3

vBleedRgb = verticalize . bleedRgb
vPixelSort = verticalize . pixelSort
--vAsdfPixelSortRgb = verticalize . asdfPixelSortRgb
vAsdfPixelSort = verticalize . asdfPixelSort

--hyperSplit :: StdGen -> Picture -> Picture
--hyperSplit g = map (drop Green . (applyRgb (bleed g1, toBlack, bleed g3)))
--  where (g1, g2, g3) = split3 g

shift :: StdGen -> Picture -> Picture
shift g = applyRowFnsToImage (repeatElems (randomRs (1, 10) g1) rowFns)
  where (g1, g2, g3) = split3 g
        rowFns :: [[Picxel] -> [Picxel]]
        rowFns = zipWith fn (randomRs (-360, 360) g2) (randoms g3)
        fn :: Int -> ColorComponent -> ([Picxel] -> [Picxel])
        fn a b = (rotate a) . map (keep b)

rgbShiftElm :: Int -> StdGen -> Picture -> Picture
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
