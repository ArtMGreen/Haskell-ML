{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

-- | Example I used:
-- https://github.com/HuwCampbell/grenade/blob/master/examples/main/mnist.hs


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random

import qualified Data.Vector.Storable as V

import           Codec.Picture

import           Grenade
import           Grenade.Utils.OneHot

-- | Blueprint for the neural net
-- Convolution channels filters kernelRows kernelCols strideRows strideCols
-- Pooling kernelRows kernelCols strideRows strideCols
-- out = (in - kernel) / stride + 1
type PizzaOfflineBlueprint
    = Network            -- Input sides: 100x100
        '[ Convolution  3  4 9 9 1 1    -- 92x92x4
         , Pooling           2 2 2 2    -- 46x46x4
         , Relu
         , Convolution  4  8 8 8 1 1    -- 39x39x8
         , Pooling           3 3 3 3    -- 13x13x8
         , Elu
         , Convolution  8 16 5 5 1 1    -- 9x9x16
         , Pooling           3 3 3 3    -- 3x3x16
         , Relu
         , Reshape                      -- 144
         , FullyConnected 144 24        -- 24
         , Logit
         , FullyConnected 24 2          -- 2
         , Softmax]                     -- 2
        '[ 'D2 100 100, 'D3 92 92 4, 'D3 46 46 4, 'D3 46 46 4
         , 'D3 39 39 8, 'D3 13 13 8, 'D3 13 13 8, 'D3 9 9 16
         , 'D3 3 3 16, 'D3 3 3 16, 'D1 144, 'D1 24, 'D1 24, 'D1 2, 'D1 2]

-- | Initializing with random weights
randomPizzaOffline :: MonadRandom m => m PizzaOfflineBlueprint
randomPizzaOffline = randomNetwork

pizzaOptions = LearningParameters
               <$> 0.01 -- train rate
               <*> 0.9  -- momentum (unimplemented in lib)
               <*> 0.0005 -- L2

-- | Learning function, basically
learn
    :: Int
    -> Network layers shapes
    -> [FilePath]
    -> [FilePath]
    -> LearningParameters
    -> IO (Network layers shapes)
learn 0 network _ _ _ = return network
learn iterations network pizzas notPizzas parameters = do
    let currNet = network
    -- exampleinIO :: IO (S ('D3 100 100 3), S ('D1 2))
    exampleinIO <- findPizza pizzas notPizzas
    -- (example, label) :: (S ('D3 100 100 3), S ('D1 2))
    (example, label) <- exampleinIO
    let gradients = backPropagate currNet example label
    let currNet = applyUpdate parameters currNet gradients
    return (learn currNet (iterations - 1) pizzas notPizzas parameters)

-- | Eat input pizzas and go crazy
main :: IO ()
main = do
    result <- learn 15 randomPizzaOffline [""] [""] pizzaOptions
    case result of
        Right () -> pure ()
        Left err -> putStrLn err

-- | Check for pizzas in the input directory
findPizza
    :: [FilePath]
    -> [FilePath]
    -> [IO (S ('D3 100 100 3), S ('D1 2))]
findPizza pizzaPaths nonPizzaPaths = do
    let pizzaList = map (\path -> parsePizza path 1) pizzaPaths
    let nonPizzaList = map (\path -> parsePizza path 0) nonPizzaPaths
    concat pizzaList nonPizzaList

-- | Flatten a 100x100 RGB8 image into an Int list
pixelRunner :: Image PixelRGB8 -> Int -> Int -> [Int]
pixelRunner pic 100 100 = []
pixelRunner pic 100 y = pixelRunner pic 0 (y+1)
pixelRunner pic x y = r : g : b : pixelRunner pic (x+1) y
    where decompose (PixelRGB8 r g b) = (r, g, b)
          (r, g, b) = decompose (pixelAt pic x y)

-- | Unbox a pizza to eat
parsePizza :: FilePath -> Int -> IO (S ('D3 100 100 3), S ('D1 2))
parsePizza path flag = do
    Just label <- oneHot flag
    maybeDynImg <- readImage path
    case maybeDynImg of
        Left error -> putStrLn error
        Right dynImg -> do
            -- pic :: Image PixelRGB8
            let pic = convertRGB8 dynImg
            let pixelList = pixelRunner pic 0 0
            let pizza = (fromStorable . V.fromList) (map fromIntegral pixelList)
            return (pizza, label)
