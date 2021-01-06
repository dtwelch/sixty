{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
module Main where

import Protolude hiding (check, force, option)

import qualified Data.Text as Text
import Options.Applicative

import qualified Command.BenchmarkProjectGenerator
import qualified Command.Check as Command
import qualified Command.Compile as Command
import qualified Command.Watch as Command
import qualified LanguageServer

main :: IO ()
main =
  join $ execParser optionsParser

optionsParser :: ParserInfo (IO ())
optionsParser =
  info (helper <*> commands)
    $ fullDesc
    <> progDesc "Sixten compiler"
    <> header "sixten"

commands :: Parser (IO ())
commands = subparser
  $ command "check" checkCommand
  <> command "watch" watchCommand
  <> command "compile" compileCommand
  <> command "language-server" languageServerCommand
  <> command "generate-benchmark-project" generateBenchmarkCommand

languageServerCommand :: ParserInfo (IO ())
languageServerCommand =
  info (pure LanguageServer.run)
    $ fullDesc
    <> progDesc "Start a language server"
    <> header "sixten language-server"

checkCommand :: ParserInfo (IO ())
checkCommand =
  info
    (helper <*>
      (Command.check
        <$> inputFiles
        <*> switch (
          long "print-elaborated"
          <> help "Print elaborated syntax after type checking"
        )
      )
    )
    $ fullDesc
    <> progDesc "Type check a Sixten program"
    <> header "sixten check"

watchCommand :: ParserInfo (IO ())
watchCommand =
  info (helper <*> (Command.watch <$> inputFiles))
    $ fullDesc
    <> progDesc "Type check a Sixten program, watching for changes"
    <> header "sixten watch"

compileCommand :: ParserInfo (IO ())
compileCommand =
  info (helper <*> (Command.compile <$> inputFiles))
    $ fullDesc
    <> progDesc "Compile a Sixten program"
    <> header "sixten compile"

generateBenchmarkCommand :: ParserInfo (IO ())
generateBenchmarkCommand =
  info
    (helper <*>
      fmap Command.BenchmarkProjectGenerator.generate
        (Command.BenchmarkProjectGenerator.Options
          <$> strArgument
            (metavar "FILE"
            <> help "Output directory"
            <> action "directory"
            )
          <*> option auto
            (long "modules"
            <> metavar "COUNT"
            <> help "Generate COUNT modules (default: 100)"
            <> value 100
            )
          <*> option auto
            (long "imports"
            <> metavar "COUNT"
            <> help "Generate at most COUNT imports per module (default: 10)"
            <> value 10
            )
          <*> option auto
            (long "functions"
            <> metavar "COUNT"
            <> help "Generate COUNT functions per module (default: 30)"
            <> value 30
            )
        )
    )
    $ fullDesc
    <> progDesc "Type check a Sixten program, watching for changes"
    <> header "sixten watch"

inputFiles :: Parser [FilePath]
inputFiles =
  many
    $ strArgument
    $ metavar "FILES..."
    <> help
      (toS $ Text.unlines
        [ "Input source files, project files, or directories."
        , "If no files are given, I will look for a 'sixten.json' file in the current directory and its parent directories."
        ]
      )
    <> action "file"
