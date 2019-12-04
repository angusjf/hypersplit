module HyperSplit where

import qualified Graphics.Image as I
import System.Environment
import System.Random
import Utils

run :: (StdGen -> Picture -> Picture) -> String -> IO ()
run fn infile = do
        img <- I.readImageRGB I.VU infile
        g <- getStdGen
        I.displayImage $ fn g img

horizontalBleedRgb :: StdGen -> Picture -> Picture
horizontalBleedRgb g = applyRgb (bleed g1, bleed g2, bleed g3)
  where (g1, g2, g3) = split3 g

verticalBleedRgb :: StdGen -> Picture -> Picture
verticalBleedRgb g = I.transpose . horizontalBleedRgb g . I.transpose

horizontalBleedRb :: StdGen -> Picture -> Picture
horizontalBleedRb g = applyRgb (bleed g1, toBlack, bleed g3)
  where (g1, g2, g3) = split3 g

horizontalBleedR :: StdGen -> Picture -> Picture
horizontalBleedR g = applyRgb (bleed g1, toBlack, id)
  where (g1, g2, g3) = split3 g

bleed :: StdGen -> Picture -> Picture
bleed g img = (I.fromLists . fn . I.toLists) img
  where fn = repeatElems2D (randomRs (5, 15) g)

toBlack :: Picture -> Picture
toBlack = I.map (const black)
  where black :: Picxel
        black = I.PixelRGB 0 0 0

