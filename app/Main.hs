{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Grammar
import TransducerToDot
import qualified P4TransducerToDot as P4Dot

import qualified CodeGen.AName as AName
import GrammarToTransducer
import Data.GraphViz.Commands
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS
import Data.GraphViz.Types hiding (parse)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.Lazy as TL
import Parser
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import qualified CodeGen.Continuation as C

data Settings = Settings {
  debug :: Bool,
  optimize :: Bool,
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
  optimize = False,
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
    , Option ['p'] ["opt"]
        (NoArg (\s -> s { optimize = True }))
        "Enable optimizations"

   ,Option ['b'] ["backend"]
        (ReqArg (\x s -> s { backend = x, codegen = backendFromString x }) "BACKEND")
        "Backend code generator (continuations, stacks (do not use))"
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
    "continuations" -> error "tmp"
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
    (Left e) -> putStrLn $ Parser.prettyError e
    (Right ddg) -> do 
      print $ show ddg
      print $ pp_Grammar ddg
      let transducer = keepOnlyReachable .update_output_transitions . removeDuplicateEdges . epsilon_elimination . grammar_to_transducer $ ddg

      case backend settings of
        "continuations" -> do
          let (p4t, c) = C.transducer_to_p4 ddg $ C.format transducer
          let o = C.optimize p4t []
          when (debug settings) $ do
            putStrLn "Original abstract P4"
            print p4t
            putStrLn "Optimized abstract P4"
            print $ o

          let dotgraph = (transducerToGraph transducer)
          _ <- runGraphvizCommand Dot dotgraph Png ( "debug_original_transducer"++ ".png")
          
          print "p4t"
          print p4t

          print""
          let dotgraph2 = P4Dot.p4TransducerToGraph p4t
          _ <- runGraphvizCommand Dot dotgraph2 Png ("debug_p4transducer" ++ ".png")

          let dotgraph2 = P4Dot.p4TransducerToGraph o
          _ <- runGraphvizCommand Dot dotgraph2 Png ("debug-p4transducer-opt" ++ ".png")
          let dotOutput = printDotGraph dotgraph2
          writeFile ("debug-p4transducer-opt" ++ ".dot") (TL.unpack dotOutput)
          
          when (optimize settings) $ do
            putStrLn "Optimized P4 code"
            let code = C.to_p4 ddg c o

            putStrLn $ code
          
          when (not $ optimize settings) $ do
            putStrLn "P4 code"
            let code = C.to_p4 ddg c p4t
            putStrLn $ code


          putStrLn "done"


        "stacks" -> do

            let (c,p) =  AName.gen_parser () ((AName.empty_context ddg) {AName.indent = 1}) transducer

            let dotgraph = (transducerToGraph transducer)
            _ <- runGraphvizCommand Dot dotgraph Png ( "debug_transducer"++ ".png")

            let dotOutput = printDotGraph dotgraph
            writeFile ("debug_transducer" ++ ".dot") (TL.unpack dotOutput)

            case output_file settings of
              Just outFile -> writeFile outFile p
              Nothing -> putStrLn p

            let (p4t, c) =  C.transducer_to_p4 ddg $ C.format transducer
            print p4t
            print $ C.optimize p4t []


            let dotgraph2 = (P4Dot.p4TransducerToGraph p4t)
            _ <- runGraphvizCommand Dot dotgraph2 Png ( "debug_p4transducer"++ ".png")
            print "done"