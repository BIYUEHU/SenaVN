{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Brick
-- import Brick.Widgets.Core
-- import Brick.Widgets.List
-- import qualified Graphics.Vty as V

import Data.Text (Text, pack)
import Data.Text.IO as TIO
import qualified Models ()
import Options.Applicative as O

data Command = Add {name :: Text, path :: Maybe Text} | Stats {id :: Int}

commandParser :: Parser Command
commandParser =
  subparser $
    command "add" (info addParser (progDesc "Add game"))
      <> command "stats" (info statsParser (progDesc "Show stats"))
  where
    addParser =
      Add
        <$> argument str (metavar "NAME")
        <*> optional (strOption (short 'p' <> long "path" <> help "Exe path" <> metavar "PATH"))

    statsParser = Stats <$> option auto (long "id" <> help "Game ID")

main :: IO ()
main = execParser opts >>= handle
  where
    opts = info (commandParser <**> helper) (fullDesc <> progDesc "Gal Manager")
    handle (Add t p) = TIO.putStrLn $ pack "Added: " <> t <> maybe "" (\x -> " (" <> x <> ")") p
    handle (Stats i) = TIO.putStrLn $ "Stats for ID " <> pack (show i)
