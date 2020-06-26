module Main (main) where

import qualified Graphics.Image as I
import System.Environment
import System.Random
import Algorithms

nothing = flip const
reapply = applyTo "output.png"

main :: IO ()
main = do 
    [infile] <- getArgs
    applyTo infile bleed

applyTo :: String -> Algorithm -> IO ()
applyTo infile fn = do
    img <- I.readImageRGB I.VU infile
    g <- getStdGen
    I.writeImage "outfile.png" (fn g img)
