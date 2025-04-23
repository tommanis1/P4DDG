{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module CodeGen.Continuations where

import qualified Transducer.Def as T
import P4Types
import Data.Graph.Inductive hiding(Node, Edge, Graph)
import qualified DDG.P4DDG hiding (Stmt)
import DDG.Types (Grammar, Label(..), Rule(..), Param(..), Nonterminal(..))
type P4Transducer =
    T.Transducer
    ([Stmt P4Types.Statement])
    (Transition (DDG.P4DDG.E P4Types.Expression))

data Transition e =
    Otherwise | E e deriving (Show)

data Stmt stmt =
    Push Integer
    | Pop Integer
    | Extract String
    | Bind String String
    | HostLanguageStmts [stmt] deriving (Show)

mkP4Transducer :: Grammar->T.P4Transducer -> P4Transducer
mkP4Transducer g t =
    let
        formatted = T.format t
        new_graph = T.graph $ foldl plus
            T.Transducer{T.start=0, T.graph=mkGraph [] []}
            (map (convert g) formatted)
    in
        T.Transducer{T.start = T.start t, T.graph = new_graph}

mkGraph' nodes edges = T.Transducer{T.start = 0, T.graph = mkGraph nodes edges}

convert :: Grammar -> (Int, [(T.Transition (DDG.P4DDG.E P4Types.Expression), Int)]) -> P4Transducer
convert grammar tranducer_state =
    case tranducer_state of
        (stateId, [(T.Labeled (Terminal t), s2)]) ->
            let
                nodes = [(stateId, [Extract t]), (s2, []) ]
                edges = [(stateId, s2, Otherwise)]
            in mkGraph' nodes edges
        (stateId, [(T.Labeled (Statements s), s2) ]) ->
            let
                nodes = [(stateId, [HostLanguageStmts s]), (s2, []) ]
                edges = [(stateId, s2, Otherwise)]
            in mkGraph' nodes edges
        (stateId, [(T.Labeled (NonTerminalCall fname1 e1), s2), (T.Return fname2 e2, s3)]) ->
            if fname1 == fname2 && e1 == e2 then
                let
                    continuation = 111
                    ps :: [String] = map (\(Param _ id) -> id) $ params grammar fname1
                    es :: [String] = wordsWhen (== ',') e1
                    body = map (\(a, b) -> Bind a b) (zip ps es) ++ [Push continuation]

                    nodes = [(stateId, body)]
                    edges = [(stateId, s2, Otherwise)]
                in mkGraph' nodes edges
            else
                error "Function names or args do not match"
       {-  (stateId, [(T.Output _, s2)]) ->
            let
                nodes = [(stateId, [])]
                edges = [(stateId, -1, Otherwise)]
            in mkGraph nodes edges -}
        (stateId, outgoing) ->
            let
                ifs = [o | o@(T.Labeled(Constraint _), _) <-outgoing ]
                elses = [o | o@(T.Labeled(Epsilon), _) <-outgoing ]
                rest = filter (\x -> x `notElem` ifs && x `notElem` elses) outgoing
            in
                if null rest && length elses < 2 then
                    let
                        nodes = [(stateId, [])]
                        edges =
                            map (\((T.Labeled (Constraint (e :: DDG.P4DDG.E P4Types.Expression))), s)-> (stateId, s, (E e :: Transition (DDG.P4DDG.E P4Types.Expression)))) ifs
                            ++ map (\(T.Labeled (Epsilon), s) -> (stateId, s, Otherwise)) elses
                    in mkGraph' nodes edges
                else
                    error $ show outgoing

params :: Grammar -> String -> [Param]
params g i =
    case findNonTerminal g i of
        Just (Nonterminal _ ps _) -> ps
        Nothing -> error $ "params: can't find nonterminal " ++ show i
findNonTerminal :: Grammar -> String -> Maybe Nonterminal
findNonTerminal g i =
    case filter (\(Nonterminal n _ _) -> n == i) g of
        [] -> Nothing
        (x:_) -> Just x
-- genOutgoingEdges = 
--     let
--         ifs = 
--         otherwises = 
--     in 

-- genIf :: Transition -> String
-- genIf tmpvar (If e state) = 
--     let 
--         val 
--     "if ( " ++ prt e ++ ") {\n" ++
--         tmpvar ++ " = " ++ val ++ ";\n"
--     ++ "}\n"

-- mkP4Transducer :: Transducer -> P4Transducer

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


plus :: P4Transducer -> P4Transducer -> P4Transducer
plus t1 t2 =
    let
        nodes = labNodes (T.graph t1) ++ labNodes (T.graph t2)
        edges = labEdges (T.graph t1) ++ labEdges (T.graph t2)
    in
        t1{T.graph=mkGraph nodes edges}
