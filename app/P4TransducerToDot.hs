module P4TransducerToDot where


import Data.GraphViz
import Data.GraphViz.Attributes.Complete
-- import Data.GraphViz.Types.Generalised
import Data.Graph.Inductive.Graph hiding (Node, Edge)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
-- import Data.GraphViz.Types.Generalised as G
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Set as S

import CodeGen.Continuation

import Data.List (intercalate)
-- Convert P4Transducer to GraphViz DotGraph
p4TransducerToGraph :: P4Transducer -> DotGraph Int
p4TransducerToGraph transducer =
  let
    -- Extract all states as nodes
    nodes = (map (\(state, stmts, _) -> (state, formatStmts state stmts)) transducer) ++ [(-1, "state_continue")] 
    
    -- Extract all transitions as edges
    edges = concatMap (\(state, _, transitions) -> 
             concatMap (transitionToEdge state) transitions) transducer
    
    params = nonClusteredParams {
      globalAttributes = [
        GraphAttrs [
          RankDir FromTop
        ],
        NodeAttrs [
          Style [SItem Filled []],
          Shape BoxShape,
          FillColor $ toColorList [RGB 230 243 255],
          Color $ toColorList [RGB 66 133 244],
          FontName $ TL.pack "Arial",
          Margin $ DVal 0.05
        ],
        EdgeAttrs [
          Style [SItem Bold []],
          ArrowSize 0.7,
          Color $ toColorList [RGB 102 102 102]
        ]
      ],
      fmtNode = \(n, l) -> [Label $ StrLabel $ TL.pack l],
      fmtEdge = \(_, _, l) -> [
        Label $ StrLabel $ TL.pack l,
        FontName $ TL.pack "Arial",
        FontSize 12.0
      ]
    }
  in graphElemsToDot params nodes edges

-- Format statements for display in a node
-- Format statements for display in a node
formatStmts :: State -> [Stmt] -> String
formatStmts state stmts = 
    let stmtStr = pp_stmts stmts
        -- Use \l for left alignment in GraphViz
        alignedLines = map (++"\\l") (lines ("state_" ++ show state ++ ":\n" ++ stmtStr))
    in concat alignedLines

-- Convert a transition to an edge
transitionToEdge :: State -> P4Transition -> [(Int, Int, String)]
transitionToEdge source (If expr stmts target) = 
  [(source, target, "if " ++ show expr ++ 
    (if not (null stmts) then "\n" ++ (pp_stmts stmts) else ""))]
transitionToEdge source (Goto target) = 
  [(source, target, "goto")]
transitionToEdge source Accept = 
  [(source, -1, "return")]  -- Using -1 as a special "accept" state
-- add a different name to the -1 state 
pp_stmts :: [Stmt] -> String
pp_stmts s = let x = (intercalate ";\n"$ map pp_stmt s )in if x == "" then "" else x ++ ";"

pp_stmt :: Stmt -> String
pp_stmt stmt = case stmt of
    Extract field -> "extract(" ++ field ++ ")"
    Do stmts -> intercalate "; " stmts
    CodeGen.Continuation.Params params exprs -> 
        intercalate "\n" $ map(\(p,e) ->  p ++ " = " ++ e) (zip params exprs)
        -- "params(" ++ intercalate ", " params ++ ") = (" ++ 
        -- intercalate ", " (map show exprs) ++ ")"
    Push n -> "push(" ++ show n ++ ")"