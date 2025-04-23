module TransducerToDot where

import Transducer.GrammarToTransducer
import DDG.Types hiding (Label)

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised
import Data.Graph.Inductive.Graph hiding (Node, Edge)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised as G
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Set as S
import qualified Transducer.Def as T

type Node = LNode String
type Edge = LEdge String

transducerToGraph :: T.P4Transducer -> Data.GraphViz.DotGraph Int
transducerToGraph t = 
    -- TODO differentiate between final and non-final states
    let 
        nodes = S.toList$ S.fromList $ map node $ labNodes $ T.graph t 
        e = concatMap edge $ labEdges $ T.graph t 
        params = nonClusteredParams {
            globalAttributes = [
                GraphAttrs [
                    RankDir FromLeft
                    -- , Splines LineEdges 
                    -- Splines SplineEdges
                    -- ,Overlap ScaleOverlaps
                    --,
                    -- BgColor $ [toWColor $ RGB 247 247 247]
                ],
                NodeAttrs [
                    Style [SItem Filled []],
                    Shape BoxShape,
                    FillColor $ toColorList [RGB 230 243 255],
                    Color $ toColorList [ RGB 66 133 244],
                    FontName $ TL.pack "Arial",
                    Margin $ DVal 0.05
                ],
                EdgeAttrs [
                    Style [SItem Bold []],
                    ArrowSize 0.7,
                    Color $ toColorList [ RGB 102 102 102]

                ]
            ],
            fmtNode = \(n,l) -> [Label $ StrLabel $ TL.pack l],
            fmtEdge = \(_,_,l) -> [
                Label $ StrLabel $ TL.pack l,
                FontName $ TL.pack "Arial",
                FontSize 12.0
            ]
        }
    in graphElemsToDot params nodes e
    where
        edge (s1, s2, T.Labeled l) = return $ (fromIntegral s1, fromIntegral s2, pp_l l)
        edge (s1, s2, T.Return n e) = return $ (fromIntegral $ s1, fromIntegral $ s2, "return: " ++ n ++ "(" ++ show e ++ ")")
        edge (s1, _, _) = []

        -- node (s1, s2, T.Output o) = (s1, ( show s1 ++ "\nOut:" ++ o))
        node (s1, T.Output x) = (s1, show s1 ++ " Output: " ++ show x)
        node (s1, T.NoLabel) = (s1, show s1)