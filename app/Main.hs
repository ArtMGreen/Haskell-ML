module Main where

import Codec.Picture


picturePath :: FilePath 
picturePath = "/home/artmgreen/Pictures/poolrooms_bg.jpg"

tackleImage :: (Pixel a) => Image a -> a
tackleImage img = pixelAt img 5 5

main :: IO ()
main = do
    esdi <- readImage picturePath
    case esdi of
        Left error -> putStrLn error
        Right dynImg -> print $ tackleImage (convertRGB8 dynImg)
        