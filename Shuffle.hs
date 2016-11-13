{-|
Module      : Shuffle
Description : Randomly shuffle a list
Copyright   :
License     :
Maintainer  : doug.fort@gmail.com
Stability   : experimental
Portability :

copied from https://wiki.haskell.org/Random_shuffle
-}
module Shuffle where

import System.Random
import Data.Array.IO
import Control.Monad

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- nArr n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    nArr :: Int -> [a] -> IO (IOArray Int a)
    nArr l =  newListArray (1,l)
