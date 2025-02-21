{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Grammar
import TransducerToDot
-- import PaperExampleGrammars
import GrammarToTransducer
import Data.GraphViz.Commands
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS


main :: IO ()
main = do
  let tunnel = 
        [
        (Nonterminal "header" [] (Sequence (RuleLabel $ Terminal "ethernet") (RuleLabel $ NonTerminalCall "tunnel" "ethernet.etherType")))
        , Nonterminal "tunnel" ["type"] (
            (((RuleLabel $ Constraint "type = TYPE_IPV4") `Sequence` (RuleLabel $ Terminal "ipv4"))

            `Alternation` (((RuleLabel $ Constraint "type = TYPE_MYTUNNEL") `Sequence` (RuleLabel $ Terminal "mytunnel")) `Sequence` (RuleLabel $ NonTerminalCall "tunnel" "myTunnel.protoid")))
            `Alternation` (RuleLabel Empty)
          )
        ]
  let test_cases = [
            -- bb, 
            -- b
        --   [Nonterminal "testalt" [] (
        -- Alternation (RuleLabel $ Terminal "a") (RuleLabel $ Terminal "b"))]
        tunnel
          ]
  show_test_cases test_cases

show_test_cases :: [Grammar] -> IO ()
show_test_cases g = do
  mapM_ show_test_case (zip [0..] g)


show_test_case :: (Integer, Grammar) -> IO ()
show_test_case (i, g) = do
  putStrLn "Grammar:"
  putStrLn $ pp_Grammar g
  let t_0 = grammar_to_transducer g
  let t :: Transducer =  removeDuplicateEdges $ epsilon_elimination t_0
  
  putStrLn "Transducer:"
  print t_0
  let dot = transducerToGraph t_0
  _ <- runGraphvizCommand Dot dot Png ("before-e"++ ".png")
  print $ reachable 0 (graph t_0)
  putStrLn "Transducer after e elim:"

  print t

  putStrLn "Generating transducer image."
  let f = "test" ++ show i

  let dot = transducerToGraph t
  _ <- runGraphvizCommand Dot dot Png (f ++ "after-e-elimination"++ ".png")
  -- let dot = transducerToGraph t_0
  -- _ <- runGraphvizCommand Dot dot Png (f ++ ".png")
  
  let tg =(insEdgesAndNodes [(0, 1,  Labeled Epsilon), (0, 2,  Labeled Epsilon), (3, 4,  Labeled Epsilon)] (mkGraph [(0, ()), (1, ())] [(0,0, Output "a")]) :: TransducerGraph)
  let dot = transducerToGraph (Transducer {start = 0, graph = tg })
  _ <- runGraphvizCommand Dot dot Png (f ++ "tt"++ ".png")
  print $ reachable 0 tg
  return ()
