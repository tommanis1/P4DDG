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
    nodes = map (\(state, stmts, _) -> (state, formatStmts state stmts)) transducer
    
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
formatStmts :: State -> [Stmt] -> String
formatStmts state stmts = 
  show state ++ "\n" ++ 
  intercalate "\n" (map show stmts)

-- Convert a transition to an edge
transitionToEdge :: State -> P4Transition -> [(Int, Int, String)]
transitionToEdge source (If expr stmts target) = 
  [(source, target, "if " ++ show expr ++ 
    (if not (null stmts) then "\n" ++ intercalate "\n" (map show stmts) else ""))]
transitionToEdge source (Goto target) = 
  [(source, target, "goto")]
transitionToEdge source Accept = 
  [(source, -1, "accept")]  -- Using -1 as a special "accept" state