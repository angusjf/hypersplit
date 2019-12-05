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

verticalPixelSort :: StdGen -> Picture -> Picture
verticalPixelSort _ = I.transpose . id . I.transpose

--hyperSplit :: StdGen -> Picture -> Picture
--hyperSplit g = map (drop Green . (applyRgb (bleed g1, toBlack, bleed g3)))
--  where (g1, g2, g3) = split3 g

applyRowFnsToImage :: [[Picxel] -> [Picxel]] -> Picture -> Picture
applyRowFnsToImage fns = I.fromLists . applyFnsToRows fns . I.toLists

bleed :: StdGen -> Picture -> Picture
bleed g = applyRowFnsToImage (map repeatElems rands2d)
  where rands2d = map (randomRs (5, 15)) (infinisplit g)

toBlack :: Picture -> Picture
toBlack = I.map (const black)
  where black :: Picxel
        black = I.PixelRGB 0 0 0
