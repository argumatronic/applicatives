module Main where
  
import Data.Char (isAlpha)
import Data.List (sort)

-- this tests to see if two sorted strings are equal
isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

-- `null` returns True if it's an empty string, False if it's not
-- `all isAlpha` returns False if there are any nonalphabetic
-- characters in the string, True if they are all alphabetic
maybeWord :: String -> Maybe String
maybeWord xs = 
    case null xs of
        True -> Nothing
        False -> do
            case (all isAlpha xs) of
                False -> Nothing
                True -> Just xs

-- this is just a utility to print the error message that we want
-- to be able to distinguish between "not valid input" and "not anagrams"
display :: Maybe Bool -> IO ()
display maybeAna =
    case maybeAna of
        Nothing    -> putStrLn "This is not valid input."
        Just False -> putStrLn "These are not anagrams."
        Just True  -> putStrLn "These words are anagrams."

-- note: there are a few versions of `main` in here
-- they all work but they are commented out so you can
-- try running each one and compare results for yourself

-- monadic do-within-let
main :: IO ()
main = do
    putStrLn "Please enter a word."
    firstWord <- getLine
    putStrLn "Please enter a second word."
    secondWord <- getLine
    let maybeAna = do
            first  <- maybeWord firstWord
            second <- maybeWord secondWord
            return (isAnagram first second)
    display maybeAna

-- but the monadic context isn't really necessary
-- because first and second don't depend on each other


-- so we can rewrite that with applicative
-- main :: IO ()
-- main = do
--     putStrLn "Please enter a word."
--     firstWord <- getLine
--     putStrLn "Please enter a second word."
--     secondWord <- getLine
--     let maybeAna = isAnagram 
--                    <$> (maybeWord firstWord) 
--                    -- Nothing | Just firstWord
--                    <*> (maybeWord secondWord)
--                    -- Nothing | Just secondWord
--     display maybeAna

-- -- we can even rewrite it on one line if we're into that kind of thing
-- main :: IO ()
-- main = do
--     putStrLn "Please enter a word."
--     firstWord <- getLine
--     putStrLn "Please enter a second word."
--     secondWord <- getLine
--     display (isAnagram <$> (maybeWord firstWord) <*> (maybeWord secondWord))
