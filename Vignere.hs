module Vignere
    ( splitIntoAlphabets
    , analyseAlphabet
    , frequency
    , indexOfCoincidence
    ) where

import Data.Vector(ifilter, toList, fromList)
import Data.Char(toUpper)

splitIntoAlphabet :: [Char] -> Int -> Int -> [Char]
splitIntoAlphabet cipherText keyLength pos = toList $ ifilter(predicate) cipherTextVector
    where cipherTextVector = fromList cipherText
          predicate = \x _ -> x `mod` keyLength == pos

splitIntoAlphabets :: String -> Int -> [String]
splitIntoAlphabets cipherText keyLength = map (split) indexes
    where indexes = [0..(keyLength -1)]
          split = splitIntoAlphabet cipherText keyLength

analyseAlphabet :: [Char] -> [(Char, Double)]
analyseAlphabet cipherText = map (\letter -> (letter, findNumOf letter)) letters
    where letters = ['A'..'Z']
          findNumOf = fromIntegral . frequency cipherText

frequency :: [Char] -> Char -> Int
frequency text target = length $ filter(==target) $ map (toUpper) text

indexOfCoincidence :: [Char] -> Double
indexOfCoincidence cipherText = numerator / fromIntegral denominator
    where frequencyInCiphertext = analyseAlphabet cipherText
          space = length cipherText
          denominator = space * (space - 1)
          numerator = foldl (+) 0.0 $ map(\(_, freq) -> freq * (freq - 1.0)) frequencyInCiphertext
