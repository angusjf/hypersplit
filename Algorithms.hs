module HyperSplit where

import qualified Graphics.Image as I
import System.Random
import Data.List
import Utils

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
bleed g = applyRowFnsToImage (map repeatElems rands2d)
  where rands2d = map (randomRs (5, 15)) (infinisplit g)

bleedRgb :: StdGen -> Picture -> Picture
bleedRgb g = applyRgb (bleed g1, bleed g2, bleed g3)
  where (g1, g2, g3) = split3 g

pixelSort :: StdGen -> Picture -> Picture
pixelSort g = applyRowFnsToImage rowFns
  where rowFns = map (\gen -> roughSortBy gen (4, 36) sortFn) (infinisplit g)
        sortFn = \a b -> compare (brightness a) (brightness b)

asdfPixelSort :: (Picxel -> Bool) -> Picture -> Picture
asdfPixelSort break = applyRowFnsToImage (repeat (asdfRowSort break))

pixelSortRgb :: StdGen -> Picture -> Picture
pixelSortRgb g = applyRgb (pixelSort g1, pixelSort g2, pixelSortRgb g3)
  where (g1, g2, g3) = split3 g

vBleedRgb = verticalize . bleedRgb
vPixelSort = verticalize . pixelSort
vPixelSortRgb = verticalize . pixelSortRgb
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
