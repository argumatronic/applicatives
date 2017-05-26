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
  putStrLn $ transform $  -- try not to fret about the dollar sign; here it's just like wrapping everything to its right in parentheses
    "Enjoy the snow, " ++ name opts ++ "!"
  where
    transform = if excited opts then map toUpper else id
-- switch to all caps if the "excited" option is True

main :: IO ()
main = execParser opts >>= runWithOptions
-- execParser :: ParserInfo a -> IO a
  where
    parser = Welcome <$> argument str (metavar "NAME")  -- the metavar sets usage information
                     <*> switch (short 'e' <>
                               long "excited")
    opts = info parser mempty
-- info :: Parser a -> InfoMod a -> ParserInfo a
-- opts gives us a ParserInfo Welcome
-- >>= :: Monad m => m a -> (a -> m b) -> m b
-- execParser opts >>= runWithOptions
-- takes opts (:: ParserInfo Welcome) and returns an IO Welcome value as the `m a` (IO is the m)
-- runWithOptions needs the Welcome value from IO Welcome as its argument, returns an IO (), so it's (a -> m b) (IO is the m)


-- to run
-- ~/teaching/applicatives/optex$ stack exec -- optex "julie"
-- Enjoy the snow, julie!
-- ~/teaching/applicatives/optex$ stack exec -- optex "julie" -e
-- ENJOY THE SNOW, JULIE!

-- note: `help` isn't enabled here but can be

