module Main (main) where

import HyperSplit
import HyperSplit.Algorithms
import System.Environment
import Data.Maybe
import Text.Read (readMaybe)

main :: IO ()
main = do 
    args <- getArgs
    case args of
      ["--help"] ->
        putStrLn "usage: hypersplit algorithm_name image.png" >>
        putStrLn "where algorithm is: nothing, bleed, bleedRgb, pixelSort, " >>
        putStrLn "vBleedRgb, vPixelSort, shift, rgbShiftElm N"
      _:_:_ ->
        let infile:algoName = args
            algorithm = toAlgorithm algoName
        in case algorithm of
             Just alg -> applyTo infile alg
             Nothing -> putStrLn "error: bad algorithm. see --help"
      _ ->
        putStrLn "usage: hypersplit algorithm_name image.png. see --help"

toAlgorithm :: [String] -> Maybe Algorithm
toAlgorithm ["nothing"] = Just nothing
toAlgorithm ["bleed"] = Just bleed
toAlgorithm ["bleedRgb"] = Just bleedRgb
toAlgorithm ["pixelSort"] = Just pixelSort
toAlgorithm ["vBleedRgb"] = Just vBleedRgb
toAlgorithm ["vPixelSort"] = Just vPixelSort
toAlgorithm ["shift"] = Just shift
toAlgorithm ["rgbShiftElm", n] = readMaybe n >>= Just . rgbShiftElm
toAlgorithm _ = Nothing
