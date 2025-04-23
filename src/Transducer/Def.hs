module Transducer.Def where
import DDG.Types (Label(..), NonTerminalId)
import Data.Graph.Inductive hiding(Node, Edge, Graph)
import P4Types
import DDG.P4DDG

data Transducer l el = Transducer {
    start :: Int
    , graph :: Gr l el
    } deriving (Show)

type P4Transducer = Transducer StateLabel (Transition (DDG.P4DDG.E P4Types.Expression))

data Transition e = 
    Labeled (Label e)
    | Return String String deriving (Show, Eq, Ord)

data StateLabel = Output NonTerminalId | NoLabel deriving (Show, Eq, Ord)

type State = Int
type TransducerFormatted = [(State, [(Transition (DDG.P4DDG.E P4Types.Expression), State)])]
format :: P4Transducer -> TransducerFormatted
format transducer =
    let
        nodes = labNodes (graph transducer)
    in
        map (findEdges (graph transducer) ) nodes
    where
        findEdges :: Gr a b -> LNode a -> (State, [(b, State)])
        findEdges graph node =
            let o = lsuc graph (to_state node) in
            (to_state node , map (\(node_id,label) -> (label, node_id)) o )

        to_state :: LNode a -> State
        to_state (node_id, _) = node_id