{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- imports {{{

import           Control.Monad         (replicateM)
import           Control.Monad.Reader  (ReaderT, asks, liftIO, runReaderT)
import           Data.Semigroup        ((<>))
import           Data.Time.Clock       (getCurrentTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Options.Applicative
import qualified System.Random         as Random
import           Text.Read             (readMaybe)

-- }}}

data Options = Options
  { prefix        :: String
  , randomLength  :: Int
  , timePrecision :: Int }

-- Basics for encoding unique IDs {{{

alpha :: [Char]
alpha = ['A'..'Z'] ++ ['a'..'z']

numeric :: [Char]
numeric = ['0'..'9']

charset :: [Char]
charset = numeric ++ alpha

-- }}}

-- Custom-precision, encoded time part {{{

encodeTime :: [Char] -> Integer -> [Char] -> [Char]
encodeTime charset timestamp str =
  if timestamp == 0
     then str
     else encodeTime charset (timestamp `div` charsetLen) (encodedChar : str)
  where
    charsetLen = toInteger $ length charset
    index = fromInteger $ timestamp `rem` charsetLen
    encodedChar = charset !! index

timeMS :: ReaderT Options IO String
timeMS = do
  precision <- asks timePrecision
  now <- round . (*(10 ^ precision)) <$> liftIO getPOSIXTime
  return $ encodeTime charset now ""

-- }}}

-- Random part {{{

randomPart :: ReaderT Options IO [Char]
randomPart = do
  numRandomChars <- asks randomLength
  liftIO $ replicateM numRandomChars randomizeChar
  where
    charsetLen = toInteger $ length charset
    randomizeChar = do
      randomChar <- Random.randomRIO (0, charsetLen - 1)
      return $ charset !! fromInteger randomChar

-- }}}

makePrefixedUniqueId :: ReaderT Options IO [Char]
makePrefixedUniqueId = do
  prefix <- asks prefix

  encodedTime <- timeMS
  encodedRandomPart <- randomPart

  return $ prefix ++ "-" ++ encodedTime ++ "-" ++ encodedRandomPart

-- CLI Options {{{

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
  uid <- runReaderT makePrefixedUniqueId options
  putStrLn uid
