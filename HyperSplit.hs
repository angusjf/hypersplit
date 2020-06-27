module HyperSplit where

import qualified Graphics.Image as I
import HyperSplit.Algorithms
import System.Random

reapply = applyTo "output.png"

applyTo :: String -> Algorithm -> IO ()
applyTo infile fn = do
    img <- I.readImageRGB I.VU infile
    g <- getStdGen
    I.writeImage "outfile.png" (fn g img)
