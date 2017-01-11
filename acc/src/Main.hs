module Main where

import Data.List (null, sort)
import Data.Char (isAlpha)
import Data.Validation

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

maybeWord :: String -> AccValidation [String] String
maybeWord xs = 
    case null xs of
        True -> AccFailure ["Empty string."]
        False -> do
            case (all isAlpha xs) of
                False -> AccFailure ["This is not a word."]
                True -> AccSuccess xs

display :: AccValidation [String] Bool -> IO ()
display validAna =
    case validAna of
        AccFailure err   -> putStrLn (concat err)
        AccSuccess False -> putStrLn "These are not anagrams."
        AccSuccess True  -> putStrLn "These words are anagrams."

-- this version of `main` works
main :: IO ()
main = do
    putStrLn "Please enter a word."
    firstWord <- getLine
    putStrLn "Please enter a second word."
    secondWord <- getLine
    let validAna = isAnagram 
                   <$> (maybeWord firstWord) 
                   <*> (maybeWord secondWord)
    display validAna

-- this one does not work because no Monad instance
-- main :: IO ()
-- main = do
--     putStrLn "Please enter a word."
--     firstWord <- getLine
--     putStrLn "Please enter a second word."
--     secondWord <- getLine
--     let validAna = do
--             first  <- maybeWord firstWord
--             second <- maybeWord secondWord
--             pure $ isAnagram first second
--     display validAna

-- to get `do` syntax that works with AccValidation, see applicDo.hs