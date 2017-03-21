import Input
import Vignere

import Data.List(sortBy)
import Data.Ord(comparing, Down(..))

average :: [Double] -> Double
average x = sum / (f len)
    where sum = foldl (+) 0.0 x
          len = length x
          f = fromIntegral

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (comparing Down)

calcIoC :: [Char] -> Int -> String
calcIoC cipherText 1    = show $ indexOfCoincidence cipherText
calcIoC cipherText len  = show $ map (indexOfCoincidence) $ splitIntoAlphabets cipherText len

readCipherTextFromFile :: String -> String
readCipherTextFromFile x = "Whoa, stubbed functions!"

takeAction :: (Char, [String]) -> (a -> b, [String])
takeAction (cmd, args)
  | cmd == 'R'  = (readCipherTextFromFile, [head args])
  | cmd == 'I'  = (\ct args -> calcIoC ct $ stringToInt args, [head args])
  | otherwise   = (show, ["Type '?' for help"])


main :: IO ()
main = do
    command <- getLine

    let action = takeAction $ parseCommand command
    putStrLn action

    putStrLn "Ciphertext:"
    unsanitisedCiphertext <- getLine
    let santisedCiphertext = sanitiseInput unsanitisedCiphertext
        alphabets = map (splitIntoAlphabets santisedCiphertext) [1..10]
        iocs = map (\x -> (length x, map indexOfCoincidence x)) alphabets
        averages = map (\(x, y) -> (x, average y)) iocs
    putStrLn $ show averages
    main
