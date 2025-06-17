module Main where

-- import DDG.Types
-- import CodeGen.Continuations hiding (id)
-- import Transducer.GrammarToTransducer

-- import qualified Data.Text.Lazy as TL
import Parser
import Text.Megaparsec hiding (State)
-- import Text.Megaparsec.Char
-- import qualified Data.Text as T
-- import qualified Text.Megaparsec.Char.Lexer as L
-- import Transducer.Def(format)
import CodeGen.Continuations
import Transducer.GrammarToTransducer

import qualified Dot.P4TransducerToDot as P4IrDot
import qualified Dot.TransducerToDot as Dot

import GHC.JS.Foreign.Callback
import GHC.JS.Prim

import Data.GraphViz.Commands
import Data.GraphViz.Types hiding (parse)
import qualified Data.Text.Lazy as T

foreign import javascript "((f) => { globalCompile = f })"
  compile :: Callback (JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
  log <- syncCallback1' (\x -> main' x)
  compile log
  -- Don't release the callback since we want it to stay available for the webpage

-- foreign import javascript "((pairs) => { return Object.fromEntries(pairs); })"
--   buildObjectFromPairs :: [(JSVal, JSVal)] -> IO JSVal

main' :: JSVal -> IO JSVal
main' x = 
  let 
    input = fromJSString x
    s = toJSString ""
  in 
      case parse parse_Grammar "" input of 
    (Left e) -> do 
      putStrLn  $ Parser.prettyError e
      toJSArray [toJSString "", s, s, s]
      
    (Right ddg) -> do 
      let transducer = grammar_to_transducer $ ddg
      let (p4t, c) = mkP4Transducer ddg transducer
      let p4code = mkP4 ddg p4t c

      let transducerDot = T.unpack . printDotGraph . Dot.transducerToGraph $ transducer
      toJSArray [s, toJSString p4code,  toJSString transducerDot, s]
      -- buildObjectFromPairs [(toJSString "a", toJSString "a")]
      
      -- toJSString transducerDot

-- main :: IO ()
-- main = do
--   putStrLn "Hello, World!"
-- main = do
--   input <- getContents
--   case parse parse_Grammar "" input of 
--     (Left e) -> do 
--       putStrLn $ Parser.prettyError e
      
--     (Right ddg) -> do 
--       let transducer = grammar_to_transducer $ ddg
--       let (p4t, c) = mkP4Transducer ddg transducer
--       let p4code = mkP4 ddg p4t c
--       putStrLn p4code