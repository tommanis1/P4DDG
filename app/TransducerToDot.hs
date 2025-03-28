module TransducerToDot where

import GrammarToTransducer
import Grammar (pp_l)

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


type Node = LNode String
type Edge = LEdge String


-- transducerToGraph :: Transducer -> Data.GraphViz.DotGraph Int
-- transducerToGraph t = 
--     -- TODO differentiate between final and non-final states
--     let nodes = map node $ M.toList (get_map t)
--         e = edges $ M.toList (get_map t) 
--         params = nonClusteredParams {
--             globalAttributes = [
--                 GraphAttrs [
--                     RankDir FromLeft,
--                     Splines SplineEdges,
--                     Overlap ScaleOverlaps
--                 ],
--                 NodeAttrs [
--                     Style [SItem Filled []],
--                     Shape BoxShape
--                 ],
--                 EdgeAttrs [
--                     Style [SItem Bold []],
--                     ArrowSize 0.8
--                 ]
--             ],
--             fmtNode = \(n,l) -> [Label $ StrLabel $ TL.pack l],
--             fmtEdge = \(_,_,l) -> [
--                 Label $ StrLabel $ TL.pack l,
--                 FontName  $ TL.pack $ "Helvetica",
--                 FontSize 10.0
--             ]
--         }
--     in graphElemsToDot params nodes e
--     where
--         edges :: [(StateId, [Transition])] -> [Edge]
--         edges = concatMap (\(s, ts) -> concatMap (edge s) ts)

--         edge :: StateId -> Transition -> [Edge]
--         edge s1  (Labeled l s2) = return $ (fromIntegral s1, fromIntegral s2, pp_l l)
--         edge s1 (Call e s2) = return $ (fromIntegral $ s1, fromIntegral $ s2, "call ")
--         edge s1 _ = []

--         node :: (StateId, [Transition]) -> Node
--         node (s,ts) = 
--             let m_finalLabel = find_final ts in
--             case m_finalLabel of
--                 Just l -> (fromIntegral s, l)
--                 Nothing -> (fromIntegral s, show s)

--         find_final :: [Transition] -> Maybe String
--         find_final [] = Nothing
--         find_final (Output l : _) = Just ("A:" ++ l)
--         find_final (_ : ts) = find_final ts

transducerToGraph :: Transducer -> Data.GraphViz.DotGraph Int
transducerToGraph t = 
    -- TODO differentiate between final and non-final states
    let 
        nodes = S.toList$ S.fromList $ map node $ labEdges $ graph t 
        e = concatMap edge $ labEdges $ graph t 
        params = nonClusteredParams {
            globalAttributes = [
                GraphAttrs [
                    RankDir FromLeft,
                    Splines SplineEdges,
                    Overlap ScaleOverlaps
                ],
                NodeAttrs [
                    Style [SItem Filled []],
                    Shape BoxShape
                ],
                EdgeAttrs [
                    Style [SItem Bold []],
                    ArrowSize 0.8
                ]
            ],
            fmtNode = \(n,l) -> [Label $ StrLabel $ TL.pack l],
            fmtEdge = \(_,_,l) -> [
                Label $ StrLabel $ TL.pack l,
                FontName  $ TL.pack $ "Helvetica",
                FontSize 10.0
            ]
        }
    in graphElemsToDot params nodes e
    where
        edge (s1, s2, Labeled l) = return $ (fromIntegral s1, fromIntegral s2, pp_l l)
        edge (s1, s2, Call e) = return $ (fromIntegral $ s1, fromIntegral $ s2, "call ")
        edge (s1, _, _) = []

        node (s1, s2, Output o) = (s1, ( show s1 ++ "\nOut:" ++ o))
        node (s1, _, _) = (s1, show s1)