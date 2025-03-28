{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Grammar
import TransducerToDot
import qualified CodeGen.AName as AName
import GrammarToTransducer
import Data.GraphViz.Commands
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)

import Parser
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

data Settings = Settings {
  debug :: Bool,
  backend :: String,
  codegen :: Transducer -> String,
  input_file :: FilePath,
  output_file :: Maybe FilePath
}

instance Show Settings where
  show s = "Settings { debug = " ++ show (debug s) ++
           ", input_file = " ++ show (input_file s) ++
           ", output_file = " ++ show (output_file s) ++ 
           ", backend = " ++ show (backend s) ++
           " }"

-- GENERATED WITH AN LLM
defaultSettings :: Settings
defaultSettings = Settings {
  debug = False,
  backend = "",
  codegen = \_ -> "",
  input_file = "",
  output_file = Nothing
}

type Flag = Settings -> Settings

-- GENERATED WITH AN LLM
options :: [OptDescr Flag]
options =
    [Option ['d'] ["debug"]
        (NoArg (\s -> s { debug = True }))
        "Enable debug mode"
   ,Option ['b'] ["backend"]
        (ReqArg (\x s -> s { backend = x, codegen = backendFromString x }) "BACKEND")
        "Backend code generator (stacks)"
    ,Option ['i'] ["input"]
        (ReqArg (\x s -> s { input_file = x }) "FILE")
        "Input file"
    ,Option ['o'] ["output"]
        (ReqArg (\x s -> s { output_file = Just x }) "FILE")
        "Output file (default: stdout)"
    ,Option ['h'] ["help"]
        (NoArg (\_ -> error "Help requested"))
        "Show this help text"
    ]
backendFromString  :: String -> (Transducer -> String)
backendFromString = \case
    "stacks" -> AName.gen_p4
    x -> error $ "Invalid backend: " ++ x

-- GENERATED WITH AN LLM
argparse :: IO Settings
argparse = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, nonOpts, []) -> do
      let settings = foldl (flip id) defaultSettings flags
          finalSettings = if null nonOpts
                          then settings
                          else settings { input_file = head nonOpts }
      if input_file finalSettings == ""
        then do
          hPutStrLn stderr "Error: No input file specified"
          showHelp
          error "No input file specified"
      else if backend finalSettings == ""
        then do
          hPutStrLn stderr "Error: No backend specified"
          showHelp
          error "No backend specified"
      else return finalSettings
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs)
      showHelp
      error "Failed to parse arguments"

-- GENERATED WITH AN LLM
showHelp :: IO ()
showHelp = do
  let header = "Usage: program [OPTIONS] [input_file]"
  hPutStrLn stderr (usageInfo header options)
  exitSuccess

main :: IO ()
main = do
  settings <- argparse
  putStrLn $ "Using settings: " ++ show settings
  
  input_file_contents <- readFile $ input_file settings

  case parse parse_Grammar "" input_file_contents of
    (Left e) -> error $ show e
    (Right ddg) -> do 
      print $ show ddg
      print $ pp_Grammar ddg

      let transducer = keepOnlyReachable .update_output_transitions . removeDuplicateEdges . epsilon_elimination . grammar_to_transducer $ ddg

      let (c,p) =  AName.gen_parser () ((AName.empty_context ddg) {AName.indent = 1}) transducer
      _ <- runGraphvizCommand Dot (transducerToGraph transducer) Png ( "debug_transducer"++ ".png")

      case output_file settings of
        Just outFile -> writeFile outFile p
        Nothing -> putStrLn p


