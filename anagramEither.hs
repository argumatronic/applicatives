module Main where

import Data.Char (isAlpha)
import Data.List (sort)

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

-- this is all basically the same as the maybeAnagram version
-- but this version gives us a nicer comparison to the AccValidation
-- version that's coming next
maybeWord :: String -> Either String String
maybeWord xs = 
    case null xs of
        True -> Left "Empty string."
        False -> do
            case (all isAlpha xs) of
                False -> Left "Not a word."
                True -> Right xs

display :: Either String Bool -> IO ()
display maybeAna =
    case maybeAna of
        Left xs     -> putStrLn ("This is not valid input: " ++ xs)
        Right False -> putStrLn "These are not anagrams."
        Right True  -> putStrLn "These words are anagrams."

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
--                    -- Left String | Right firstWord
--                    <*> (maybeWord secondWord)
--                    -- Left String | Right secondWord
--     display maybeAna

-- -- we can even rewrite it on one line if we're into that kind of thing
-- main :: IO ()
-- main = do
--     putStrLn "Please enter a word."
--     firstWord <- getLine
--     putStrLn "Please enter a second word."
--     secondWord <- getLine
--     display (isAnagram <$> (maybeWord firstWord) <*> (maybeWord secondWord))
