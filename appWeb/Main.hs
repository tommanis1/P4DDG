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

import GHC.JS.Foreign.Callback
import GHC.JS.Prim


foreign import javascript "((f) => { globalCompile = f })"
  compile :: Callback (JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
  log <- syncCallback1' (\x -> compile' x)
  compile log
  -- Don't release the callback since we want it to stay available for the webpage

compile' :: JSVal -> IO JSVal
compile' x = 
  let 
    input = fromJSString x
  in 
      case parse parse_Grammar "" input of 
    (Left e) -> do 
      putStrLn  $ Parser.prettyError e
      return . toJSString $ "Error"
      
    (Right ddg) -> do 
      let transducer = grammar_to_transducer $ ddg
      let (p4t, c) = mkP4Transducer ddg transducer
      let p4code = mkP4 ddg p4t c
      return $ toJSString p4code

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