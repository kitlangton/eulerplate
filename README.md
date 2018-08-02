# Eulerplate

A tool for generating Haskell boilerplate for solving Hacker Rank challenges.

- Generates a spec file for a challenge from a challenge's problem sets.
- Generates a copy-pastable solution stub, inferring the input and output types from the challenge.

## Set Up

1. `git clone` this repo.
2. `cd` into the repo and run `stack install`.
3. Run `eulerplate --new` where you'd like to create the `hacker-rack-hs` project folder.
4. Start downloading challenges (see Example).

## Example


Say you're working on the Hacker Rank challenge [Divisble Sum Pairs](https://www.hackerrank.com/challenges/divisible-sum-pairs/problem). 

1. Identify the challenge id in the url (`divisible-sum-pairs`)
2. Execute `eulerplate --download divisible-sum-pairs` in the terminal. This will create two new files (one for your solution and the other for your spec) in your Haskell Project:

```
-- Eulerplate generated module for DivisibleSumPairs
-- Challenge url: https://www.hackerrank.com/challenges/divisible-sum-pairs/problem.
module Practice.Algorithms.Implementation.DivisibleSumPairs where

-- Write your solution in here.
-- We've attempted to parse the types from the problem sets,
-- but feel free to change it in case we messed up :)
divisibleSumPairs :: [Int] -> [Int] -> Int
divisibleSumPairs = undefined

main :: IO ()
main = do
  (a, b) <- getInput
  printOutput $ divisibleSumPairs a b

getInput :: IO ([Int], [Int])
getInput = do
  a <- fmap read . words <$> getLine :: IO [Int]
  b <- fmap read . words <$> getLine :: IO [Int]
  return (a, b)

printOutput :: Int -> IO ()
printOutput (a) = do
  print a
```

and

```
-- Eulerplate generated spec for DivisibleSumPairs
-- Challenge url: https://www.hackerrank.com/challenges/divisible-sum-pairs/problem.
module Practice.Algorithms.Implementation.DivisibleSumPairsSpec where

import Practice.Algorithms.Implementation.DivisibleSumPairs
import Test.Hspec

spec :: SpecWith ()
spec = describe "DivisibleSumPairs" $ do
  it "solves Test Case #0" $
    divisibleSumPairs [6, 3] [1, 3, 2, 6, 1, 2] `shouldBe` 5

  it "solves Test Case #12" $
    divisibleSumPairs [100, 77] [85, 42, 54, 62, 79, 71, 29, 61, 1, 92, 93, 99, 82, 5, 45, 55, 49, 49, 93, 45, 2, 53, 80, 68, 51, 15, 42, 8, 5, 45, 95, 90, 4, 5, 45, 56, 20, 66, 32, 65, 11, 48, 41, 10, 92, 41, 8, 23, 88, 50, 90, 2, 3, 88, 29, 34, 54, 83, 91, 37, 95, 11, 7, 48, 96, 2, 84, 50, 20, 97, 95, 85, 80, 87, 99, 34, 40, 33, 78, 6, 58, 82, 49, 37, 35, 12, 85, 73, 96, 7, 63, 36, 73, 3, 96, 23, 5, 75, 16, 41] `shouldBe` 44
```

3. Write your solution.
4. Run `stack test` to execute the tests.
5. Copy and paste your solution (everything except for the `module` declaration) into the browser.
6. Hooray!
