{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Grammar
import TransducerToDot
import CodeGen.AName

-- import PaperExampleGrammars
import GrammarToTransducer
import Data.GraphViz.Commands
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS

import Parser
tunnel = 
        [
        (Nonterminal "header" [] (Sequence (RuleLabel $ Terminal "hdr.ethernet") (RuleLabel $ NonTerminalCall "tunnel" "hdr.ethernet.etherType")))
        , Nonterminal "tunnel" ["type"] (
            (((RuleLabel $ Constraint "type == TYPE_IPV4") `Sequence` (RuleLabel $ Terminal "hdr.ipv4"))

            `Alternation` (((RuleLabel $ Constraint "type == TYPE_MYTUNNEL") `Sequence` (RuleLabel $ Terminal "hdr.myTunnel")) `Sequence` (RuleLabel $ NonTerminalCall "tunnel" "hdr.myTunnel.proto_id")))
            `Alternation` (RuleLabel Empty)
          )
        ]

requires_backtracking = 
  [
    Nonterminal "header" [] (
      (RuleLabel $ Terminal "hdr.e1")
      `Alternation` (RuleLabel $ Terminal "hdr.e2")
    )
  ]

nested_ambiguity = [
  Nonterminal "hdr" [] (
    (
      (Sequence (RuleLabel $ Terminal "0") (RuleLabel $ Terminal "1"))
        `Alternation` (Sequence (RuleLabel $ Terminal "0") (Sequence (RuleLabel $ Terminal "0") (RuleLabel $ Terminal "0")))
      
      `Alternation`
      ((RuleLabel $ Terminal "1")
              `Alternation` (RuleLabel $Terminal "0"))

    )
  )
  ]
main :: IO ()
main = do

  let test_cases = [
            -- bb, 
            -- b
        --   [Nonterminal "testalt" [] (
        -- Alternation (RuleLabel $ Terminal "a") (RuleLabel $ Terminal "b"))]
        tunnel
        -- ,
        -- requires_backtracking,
        -- nested_ambiguity
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
  let t :: Transducer =  update_output_transitions $ removeDuplicateEdges $ epsilon_elimination t_0
  
  putStrLn "Transducer:"
  print t
  print $ reachable (start  t) (graph t)

  -- let dot = transducerToGraph t_0
  -- _ <- runGraphvizCommand Dot dot Png ("before-e"++ ".png")
  let ttttt = keepOnlyReachable t
  putStrLn "Transducer after e elim:"

  print t

  putStrLn "Generating transducer image."
  let f = "test" ++ show i

  let dot = transducerToGraph ttttt
  _ <- runGraphvizCommand Dot dot Png (f ++ "after-e-elimination"++ ".png")
  -- let dot = transducerToGraph t_0
  -- _ <- runGraphvizCommand Dot dot Png (f ++ ".png")
  
  -- let tg =(insEdgesAndNodes [(0, 1,  Labeled Epsilon), (0, 2,  Labeled Epsilon), (3, 4,  Labeled Epsilon)] (mkGraph [(0, ()), (1, ())] [(0,0, Output "a")]) :: TransducerGraph)
  -- let dot = transducerToGraph (Transducer {start = 0, graph = tg })
  -- _ <- runGraphvizCommand Dot dot Png (f ++ "tt"++ ".png")
  -- print $ reachable 0 tg
  let (c,p) =  gen_parser () ((empty_context tunnel) {indent = 1}) t 
  putStrLn $ p
  return ()
