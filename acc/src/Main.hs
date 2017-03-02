module Main where

import Data.List (null, sort)
import Data.Char (isAlpha)
import Data.Validation

-- same as maybeEither except the AccValidation type has an
-- Applicative instance that lets it collect error messages as it goes
-- along and return more than one message -- compare to Either that only
-- tells you the first thing that went wrong
isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

maybeWord :: String -> AccValidation [String] String
maybeWord xs = 
    case null xs of
        True -> AccFailure ["Empty string."]  -- notice this isn't just a String, but a list of String
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

-- why isn't there a Monad instance? because Monad is set up to short circuit
-- on the first failure -- the first Nothing or Left you get stops the whole computation
-- Monad is structured to evaluate sequentially, in part because that's usually what you want for
-- side effecting code. Applicative is structured to be able to evaluate each side of the <*> 
-- operator independently (parallel), so with an Applicative instance such as AccValidation's,
-- it can "see" and return multiple failure results, whereas Monad can't.

-- to get `do` syntax that works with AccValidation, see applicDo.hs