#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc

module Main where

import Options.Applicative 
import Data.Char (toUpper)

data Welcome = Welcome { name :: String
                       , excited :: Bool  }
-- the first argument is a string; the switch is a Bool

runWithOptions :: Welcome -> IO ()
runWithOptions opts =
  putStrLn $ transform $
    "Enjoy the snow, " ++ name opts ++ "!"
  where
    transform = if excited opts then map toUpper else id
-- what to do if the options change

main :: IO ()
main = execParser opts >>= runWithOptions
-- execParser :: ParserInfo a -> IO a
  where
    parser = Welcome <$> argument str (metavar "NAME")
                     <*> switch (short 'e' <>
                               long "excited")
    opts = info parser mempty
-- info :: Parser a -> InfoMod a -> ParserInfo a

-- the metavar allows it to provide usage information to us
-- note: `help` isn't enabled here but can be