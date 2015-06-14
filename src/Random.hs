module Random where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import System.Random (randomRIO)
import Data.Array.ST
import GHC.Arr

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day(ModifiedJulianDay), toModifiedJulianDay)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)

import Types (Gender(..))

evalShuffle = evalRandIO . shuffle

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

uniformDateInRange :: String -> String -> IO Day
uniformDateInRange min max = do
  let parse = toModifiedJulianDay . fromJust . parseTime defaultTimeLocale "%F"
      minJulianDay = parse min
      maxJulianDay = parse max

  ModifiedJulianDay <$> randomRIO (minJulianDay, maxJulianDay)


uniformGender :: IO Gender
uniformGender = toEnum <$> randomRIO (0, 1)

uniformLocation :: IO (Double, Double)
uniformLocation = (,) <$> randomRIO (-180, 180) <*> randomRIO (-180, 180)
  
