module HyperSplit where

import qualified Graphics.Image as I
import System.Environment
import System.Random
import Data.List
import Utils

run :: (StdGen -> Picture -> Picture) -> String -> IO ()
run fn infile = do
        img <- I.readImageRGB I.VU infile
        g <- getStdGen
        I.displayImage $ fn g img

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

asdfPixelSort :: a -> Picture -> Picture
asdfPixelSort _ = applyRowFnsToImage (repeat asdfRowSort)

horizontalPixelSortRgb :: StdGen -> Picture -> Picture
horizontalPixelSortRgb g = applyRgb (horizontalPixelSort g1, horizontalPixelSort g2, horizontalPixelSortRgb g3)
  where (g1, g2, g3) = split3 g

verticalBleedRgb = verticalize . horizontalBleedRgb
verticalPixelSort = verticalize . horizontalPixelSort
verticalPixelSortRgb = verticalize . horizontalPixelSortRgb

--hyperSplit :: StdGen -> Picture -> Picture
--hyperSplit g = map (drop Green . (applyRgb (bleed g1, toBlack, bleed g3)))
--  where (g1, g2, g3) = split3 g
