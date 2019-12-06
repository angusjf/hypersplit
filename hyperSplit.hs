module HyperSplit where

import qualified Graphics.Image as I
import System.Random
import Data.List
import Utils

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

horizontalBleedRgb :: StdGen -> Picture -> Picture
horizontalBleedRgb g = applyRgb (bleed g1, bleed g2, bleed g3)
  where (g1, g2, g3) = split3 g

horizontalPixelSort :: StdGen -> Picture -> Picture
horizontalPixelSort g = applyRowFnsToImage rowFns
  where rowFns = map (\gen -> roughSortBy gen (4, 36) sortFn) (infinisplit g)
        sortFn = \a b -> compare (brightness a) (brightness b)

asdfPixelSort :: (Picxel -> Bool) -> Picture -> Picture
asdfPixelSort break = applyRowFnsToImage (repeat (asdfRowSort break))

verticalAsdfPixelSort :: (Picxel -> Bool) -> Picture -> Picture
verticalAsdfPixelSort = verticalize . asdfPixelSort

horizontalPixelSortRgb :: StdGen -> Picture -> Picture
horizontalPixelSortRgb g = applyRgb (horizontalPixelSort g1, horizontalPixelSort g2, horizontalPixelSortRgb g3)
  where (g1, g2, g3) = split3 g

verticalBleedRgb = verticalize . horizontalBleedRgb
verticalPixelSort = verticalize . horizontalPixelSort
verticalPixelSortRgb = verticalize . horizontalPixelSortRgb

--hyperSplit :: StdGen -> Picture -> Picture
--hyperSplit g = map (drop Green . (applyRgb (bleed g1, toBlack, bleed g3)))
--  where (g1, g2, g3) = split3 g
