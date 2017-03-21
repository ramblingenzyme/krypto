module Input
    ( sanitiseInput
    , stripWhiteSpace
    , parseCommand
    , stringToInt
    ) where

import Data.Char (isSpace
                 , toUpper
                 )

trimSurroundingWhitespace :: [Char] -> [Char]
trimSurroundingWhitespace = f . f
    where f = reverse . dropWhile isSpace

removeTabs :: [Char] -> [Char]
removeTabs = filter (`notElem` ['\t'])

removeEmpty :: [String] -> [String]
removeEmpty = filter (/= "")

stripWhiteSpace :: [Char] -> [Char]
stripWhiteSpace = filter (not . isSpace)

sanitiseInput :: [Char] -> [Char]
sanitiseInput = map (toUpper) . trimSurroundingWhitespace

parseCommand :: String -> (Char, [String])
parseCommand x = (toUpper $ head x, words (tail x))

stringToInt :: String -> Int
stringToInt x = read x::Int
