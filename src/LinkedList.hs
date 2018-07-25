module LinkedList where

import           Data.List (sort)

median :: [Int] -> Double
median numbers =
  if even l
    then (fromIntegral numberInMiddle + fromIntegral numberOneBeforeMiddle) / 2
    else fromIntegral numberInMiddle
  where
    l = length numbers
    middleIndex = div l 2
    numberInMiddle = numbers !! middleIndex
    numberOneBeforeMiddle = numbers !! (middleIndex - 1)

split :: [Int] -> ([Int], [Int])
split numbers = (take halfLength numbers, drop numberToDrop numbers)
  where
    len = length numbers
    isEven = even len
    halfLength = div len 2
    numberToDrop =
      if isEven
        then halfLength
        else halfLength + 1

tupleMap :: (a -> b) -> (a, a) -> (b, b)
tupleMap f (a, b) = (f a, f b)

quartiles :: [Int] -> (Double, Double, Double)
quartiles unsortedNumbers = (q1, q2, q3)
  where
    numbers = sort unsortedNumbers
    q2 = median numbers
    (q1, q3) = tupleMap median $ split numbers

main :: IO ()
main = do
  _ <- getLine
  numbers <- (fmap read) . words <$> getLine :: IO [Int]
  let (q1, q2, q3) = quartiles numbers
  print $ round q1
  print $ round q2
  print $ round q3
