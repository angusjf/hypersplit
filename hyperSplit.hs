import Prelude
import qualified Graphics.Image as I
import System.Environment
import System.Random

data ColorComponent = Red | Blue | Green
type Picture = I.Image I.VU I.RGB Double
type Picxel = I.Pixel I.RGB Double

demo :: String -> IO ()
demo infile = do
        img <- I.readImageRGB I.VU infile
        g <- getStdGen
        I.displayImage $ bleed g img

bleed :: StdGen -> Picture -> Picture
bleed g img = newRed + newGreen + newBlue
  where (red, blue, green) = rgb img
        (g1, g2, g3) = split3 g
        newRed   = bleedPart g1 red
        newGreen = bleedPart g2 green
        newBlue  = bleedPart g3 blue

rgb :: Picture -> (Picture, Picture, Picture)
rgb img = (red, green, blue)
  where red   = I.map (keep Red)   img
        blue  = I.map (keep Blue)  img
        green = I.map (keep Green) img

keep :: ColorComponent -> Picxel -> Picxel
keep Red   (I.PixelRGB r g b) = I.PixelRGB r 0 0
keep Green (I.PixelRGB r g b) = I.PixelRGB 0 g 0
keep Blue  (I.PixelRGB r g b) = I.PixelRGB 0 0 b

split3 :: StdGen -> (StdGen, StdGen, StdGen)
split3 g = (g1, g2, g3)
  where (g1, rest) = split g
        (g2, g3)   = split rest

bleedPart :: StdGen -> Picture -> Picture
bleedPart g img = I.transpose $ I.fromLists $ newLists
  where newLists = doBleedPart (randomRs (5, 15) g) lists
        lists = I.toLists $ I.transpose img
        
doBleedPart :: [Int] -> [[Picxel]] -> [[Picxel]]
doBleedPart randoms (row:rows) = bleedRow some row : doBleedPart rest rows
  where some = take (length rows) randoms
        rest = drop (length rows) randoms
doBleedPart _ []     = []

bleedRow :: [Int] -> [Picxel] -> [Picxel]
bleedRow (n:ns) (p:ps) = repeats ++ (bleedRow ns rest)
  where repeats = take (length (p:ps)) $ replicate n p
        rest    = drop n $ p:ps
bleedRow _   pixels    = pixels
