import Prelude hiding (map, drop)
import Graphics.Image hiding (invert)
import System.Environment

data ColorComponent = Red | Blue | Green

main :: IO ()
main = do
        [x, y, infile, outfile] <- getArgs
        pic <- readImageRGB VU infile
        writeImage outfile $ superHyperSplit drop 2 pic

demo :: String -> IO ()
demo infile = do
        pic <- readImageRGB VU infile
        writeImage ("1-drop-" ++ infile) $ superHyperSplit drop 1 pic
        writeImage ("2-drop-" ++ infile) $ superHyperSplit drop 2 pic
        writeImage ("3-drop-" ++ infile) $ superHyperSplit drop 3 pic
        writeImage ("4-drop-" ++ infile) $ superHyperSplit drop 4 pic
        writeImage ("1-keep-" ++ infile) $ superHyperSplit keep 1 pic
        writeImage ("2-keep-" ++ infile) $ superHyperSplit keep 2 pic
        writeImage ("3-keep-" ++ infile) $ superHyperSplit keep 3 pic
        writeImage ("4-keep-" ++ infile) $ superHyperSplit keep 4 pic

--superHyperSplit :: Int -> Image VU RGB Double -> Image VU RGB Double
superHyperSplit fn n pic = foldr (+) pic $ take n $ iterate (hyperSplit (0, 10) (+) fn) pic

hyperSplit (x, y) op keop img = red `op` blue `op` green
  where red   = move ( x,  y) . map (keop Red)   $ img
        green = move ( 0,  0) . map (keop Green) $ img
        blue  = move (-x, -y) . map (keop Blue)  $ img

move :: (Int, Int) -> Image VU RGB Double -> Image VU RGB Double
move = translate Edge

keep :: Num e => ColorComponent -> Pixel RGB e -> Pixel RGB e
keep Red   (PixelRGB r g b) = PixelRGB r 0 0
keep Green (PixelRGB r g b) = PixelRGB 0 g 0
keep Blue  (PixelRGB r g b) = PixelRGB 0 0 b

drop :: Num e => ColorComponent -> Pixel RGB e -> Pixel RGB e
drop Red   (PixelRGB r g b) = PixelRGB 0 g b
drop Green (PixelRGB r g b) = PixelRGB r 0 b
drop Blue  (PixelRGB r g b) = PixelRGB r g 0

invert :: Image VU RGB Double -> Image VU RGB Double
invert = map inver
  where inver (PixelRGB r g b) = PixelRGB (inv r) (inv g) (inv b)
        inv x = 1 - x
