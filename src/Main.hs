{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- imports {{{

import           Control.Monad         (replicateM)
import           Data.Semigroup        ((<>))
import           Data.Time.Clock       (getCurrentTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Options.Applicative
import qualified System.Random         as Random
import           Text.Read             (readMaybe)

-- }}}

-- Basics for encoding unique IDs {{{

alpha :: [Char]
alpha = ['A'..'Z'] ++ ['a'..'z']

numeric :: [Char]
numeric = ['0'..'9']

charset :: [Char]
charset = numeric ++ alpha

-- }}}

-- Custom-precision, encoded time part {{{

timeMS :: Int -> IO Integer
timeMS precision = round . (*(10 ^ precision)) <$> getPOSIXTime

encodeTime :: [Char] -> Integer -> [Char] -> [Char]
encodeTime charset timestamp str =
  if timestamp == 0
     then str
     else encodeTime charset (timestamp `div` charsetLen) (encodedChar : str)
  where
    charsetLen = toInteger $ length charset
    index = fromInteger $ timestamp `rem` charsetLen
    encodedChar = charset !! index

-- }}}

-- Random part {{{

randomPart :: [Char] -> Int -> IO [Char]
randomPart charset numChars =
  replicateM numChars randomizeChar
  where
    charsetLen = toInteger $ length charset
    randomizeChar = do
      randomChar <- Random.randomRIO (0, charsetLen - 1)
      return $ charset !! fromInteger randomChar

-- }}}

makePrefixedUniqueId :: [Char] -> Int -> Int -> IO [Char]
makePrefixedUniqueId prefix randomPartLength timePrecision = do
  now <- timeMS timePrecision
  let encodedTime = encodeTime charset now ""

  encodedRandomPart <- randomPart charset randomPartLength

  return $ prefix ++ "-" ++ encodedTime ++ "-" ++ encodedRandomPart

-- CLI Options {{{

data Options = Options
  { prefix        :: String
  , randomLength  :: Int
  , timePrecision :: Int }

positiveIntReader :: ReadM Int
positiveIntReader = eitherReader $ \arg ->
  case readMaybe arg of
    Just parsedInt | parsedInt > 0 ->
        Right parsedInt
    Just _ ->
        Left "length must be a positive number"
    Nothing -> Right 16

timePrecisionReader :: ReadM Int
timePrecisionReader = eitherReader $ \arg ->
  case readMaybe arg of
    Just parsedInt | parsedInt >= 0 && parsedInt <= 6 ->
        Right parsedInt
    Just _ ->
        Left "time precision must be an integer between 0 and 6"
    Nothing -> Right 3

argsParser :: Parser Options
argsParser = Options
  <$> strOption
    (  long "prefix"
    <> short 'p'
    <> metavar "PREFIX"
    <> help "UID prefix")
  <*> option positiveIntReader
    (  long "random-length"
    <> short 'l'
    <> value 16
    <> showDefault
    <> metavar "LEN"
    <> help "number of random characters")
  <*> option timePrecisionReader
    (  long "time-precision"
    <> short 't'
    <> value 3
    <> showDefault
    <> metavar "PRECISION")

opts :: ParserInfo Options
opts = info (argsParser <**> helper)
  (  fullDesc
  <> progDesc "Issue a prefixed UID"
  <> header "hs-uid - a haskell-based generator for prefixed unique IDs with a time component")

-- }}}

main :: IO ()
main = do
  options <- execParser opts
  uid <- makePrefixedUniqueId (prefix options) (randomLength options) (timePrecision options)
  putStrLn uid
