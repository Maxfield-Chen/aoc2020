module Main where

import Data.HashSet as S
import Text.ParserCombinators.Parsec

type MatchingNumbers = (Int, Int)

pExpenses :: Parser [Int]
pExpenses = endBy pExpense (char '\n')

pExpense :: Parser Int
pExpense = read <$> many1 digit

parseExpenses :: String -> Either ParseError [Int]
parseExpenses = parse pExpenses "(bad expense value)"

findNumbersSummingTo :: Int -> [Int] -> Maybe (Int, Int)
findNumbersSummingTo numberToSum candidates =
  let numberMap = S.fromList candidates
   in foldl
        ( \mNumbers candidate ->
            let requiredNumber = numberToSum - candidate
             in case requiredNumber `S.member` numberMap of
                  True -> pure (requiredNumber, candidate)
                  False -> mNumbers
        )
        Nothing
        candidates

main :: IO ()
main = do
  numericExpenseData <-
    readFile
      "/home/nihliphobe/projects/haskell/aoc2020/data/part1.txt"
  case parseExpenses numericExpenseData of
    Left bad -> fail (show bad)
    Right expenses -> putStrLn $ partOneMessage ++ show (findNumbersSummingTo numberToSum expenses)
  where
    numberToSum = 2020
    partOneMessage = "The two numbers that sum together are: "
