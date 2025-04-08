{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, TypeSynonymInstances , FlexibleInstances#-}

module GrammarToTransducer where

import Grammar

import Control.Monad.State.Lazy
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS

type TransducerGraph = Gr () Transition

data Transducer = Transducer {
    start :: Int
    , graph :: TransducerGraph
    } deriving (Show)

type TEdgde = (Int, Int, Transition)

-- type TransducerGraph = (M.Map StateId [Transition])
-- data  Transducer = Transducer {
--     start :: StateId
--     , transitions :: TransducerGraph
--     , reversed :: Bool
--     } deriving (Show)

data Transition = 
    Labeled Label
    | Output NonTerminalId
    | Call String Expression 
    deriving (Show, Eq, Ord)

type StateId = Int
type NonTerminalMap = M.Map NonTerminalId StateId

type UniqueId = State Int


u :: TransducerGraph -> TransducerGraph -> TransducerGraph
u g1 g2 = 
    let 
        n1 = labNodes g1
        n2 = labNodes g2

        e1 = labEdges g1
        e2 = labEdges g2
    in
        mkGraph (n1 ++ n2) (e1 ++ e2)

insEdgesAndNodes :: [(Int, Int, Transition)] -> TransducerGraph -> TransducerGraph
insEdgesAndNodes new g = 
        g `u` mkGraph nodes new

    where 
        nodes = S.toList . S.fromList $ concatMap (\(s1,s2, _) -> [(s1, ()), (s2, ())]) new

rule_to_transducer :: NonTerminalMap -> Rule -> UniqueId (StateId, StateId, TransducerGraph)
rule_to_transducer s_map (RuleLabel l) = rule_to_transducer_label s_map l
rule_to_transducer s_map (Sequence r1 r2) = do
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    (s2, f2, t2) <- rule_to_transducer s_map r2

    -- let t3 = M.fromList [(s, [Labeled Epsilon s1]), (f1, [Labeled Epsilon s2]), (f2, [Labeled Epsilon f])]
    let t3 = insEdgesAndNodes [
                (s, s1,  Labeled Epsilon)
                , (f1, s2,  Labeled Epsilon)
                , (f2, f,  Labeled Epsilon)
            ] empty

    return (s, f, t1 `u` (t2 `u` t3))



rule_to_transducer s_map (Alternation r1 r2) = do
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    (s2, f2, t2) <- rule_to_transducer s_map r2

    let t3 = insEdgesAndNodes [
                (s, s2,  Labeled Epsilon)
                , (s, s1,  Labeled Epsilon)
                , (f1, f,  Labeled Epsilon)
                , (f2, f,  Labeled Epsilon)
            ] empty

    -- let t3 = M.fromList [(s, [Labeled Epsilon s1, Labeled Epsilon s2]), (f1, [Labeled Epsilon f]), (f2, [Labeled Epsilon f])]

    return (s, f, t1 `u` (t2 `u` t3))

rule_to_transducer s_map (KleineClosure r1) = do 
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    let t3 = insEdgesAndNodes [
                (s, s1,  Labeled Epsilon)
                , (f1, f,  Labeled Epsilon)
                , (s, f,  Labeled Epsilon)
                , (f1, s1,  Labeled Epsilon)
            ] empty
    return (s, f, t1 `u` t3)


rule_to_transducer_label :: NonTerminalMap -> Label -> UniqueId (StateId, StateId, TransducerGraph)
rule_to_transducer_label s_map l@(NonTerminalCall name e) = do
    (s,f) <- fresh_2
    let m_sa = M.lookup name s_map 
    case m_sa of 
        Just sa -> 
            return (s, f, insEdgesAndNodes [(s, f, Labeled l),(s, sa, Call name e )] empty)
        Nothing ->
            error $ "Error: rule_to_transducer_label\n can't find:" ++ show name
rule_to_transducer_label s Empty = do
    (s,f) <- fresh_2
    return (s, f, mkGraph [(s, ()),(f, ())] [])
rule_to_transducer_label s l = do
    (s,f) <- fresh_2
    return (s, f, insEdgesAndNodes [(s,f, Labeled l)] empty)

fresh :: UniqueId Int
fresh = do
    lastID <- get
    put (lastID + 1)
    return lastID

fresh_2 :: UniqueId (Int, Int)
fresh_2 = do
    a <- fresh
    b <- fresh
    return (a,b)


grammar_to_transducer' :: Grammar -> UniqueId Transducer
grammar_to_transducer' grammar = do
    let s =  [(name , state_id)| ((Nonterminal name _ _), state_id) <- zip grammar [0..]]
    let s_map :: M.Map NonTerminalId Int = M.fromList s
    put (1 + length grammar )
    
    t_n :: [(Int, Int, TransducerGraph)] <- sequence $ map (rule_to_transducer s_map) $ map (\(Nonterminal _ _ r) -> r) grammar

    -- assert equal length between s and t_n
    -- T_init = [s_A0 −→ s_0]; · · · ; [s_Ak −→ s_k]

    let t_init_union_final :: TransducerGraph  =  

            insEdgesAndNodes (concat [ 
                [(state_id, s_i, Labeled Epsilon), (f_i, f_i, Output a_i) ]
                    | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]) empty
        
        -- M.fromList $ concat [ 
        --     [(state_id, [Labeled Epsilon s_i]), (f_i, [Output a_i])  ]
        --         | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]


    let t_n_ :: TransducerGraph = foldr ((u)) empty [ t | (s, f, t) <- t_n]

    return $ Transducer{ start = snd $ head s, graph = (t_init_union_final `u` t_n_)}
grammar_to_transducer :: Grammar -> Transducer
grammar_to_transducer g = evalState (grammar_to_transducer' g) 0

epsilon_closure :: StateId -> TransducerGraph -> [StateId]
epsilon_closure s t = 
    filter (\x -> x /= s) $ S.toList . S.fromList $ epsilon_closure' s t []

epsilon_closure' :: StateId -> TransducerGraph -> [StateId] -> [StateId]
epsilon_closure' s t visited = 
                let e = [target | (x, target,  Labeled Epsilon) <- labEdges t, target `notElem` visited, x == s] in
                if null e then (s:visited) else
                    concatMap (\next -> epsilon_closure' next t (s:visited)) e


update_1 :: StateId -> TransducerGraph -> TransducerGraph
update_1 s t = 
    let cl = epsilon_closure s t
        accepting ::  [(Int, Transition)] = accepting_states cl t
    in 
        if null accepting then t
        else 
            insEdgesAndNodes [(s, to, Output o) | (to, Output o) <- accepting] t
            -- make s an accepting state
            -- t `u` (M.fromList $ map (\(_, transitions ) -> (s, transitions)) (M.toList accepting))

accepting_states :: [StateId] -> TransducerGraph -> [(Int, Transition)]
accepting_states states t = 

    concatMap (outputs t) states

outputs :: TransducerGraph -> StateId ->[(Int, Transition)]
outputs t s = 
    [ (to, Output o) | e@(from, to, Output o) <- labEdges t, from == s] 
    -- case M.lookup s t of
    -- Just trans -> M.fromList [(s, [Output target | Output target <- trans])]
    -- Nothing -> M.empty

epsilon_elimination :: Transducer -> Transducer
epsilon_elimination t = 
    -- https://web.cecs.pdx.edu/~sheard/course/CS581/notes/NfaEpsilonDefined.pdf
    let 
        -- states_with_only_outgoing_epsilons = filter (\x -> null $ (outgoing [x] t)) (M.keys t)
    -- step one: mark all states that can reach an accepting state trough epsilon-transitions as accepting
        g1 = foldl (\x y -> update_1 y x) (graph t) (nodes $ graph t)
        -- Add an arc from p to q labeled a iff there is an arc labeled a in N from
        -- some state in ECLOSE(p) to q.
        g2 = foldl (\x y -> update_2 y x) (g1) (nodes g1)
    in
        -- remove_all_unreachable_states (t{graph=g1})

        remove_all_unreachable_states $ 
        t{graph = remove_epsilon_transitions g2}
        -- (Transducer s $ remove_epsilon_transitions $ t2)
        -- error $ "AAAAAAA" ++ show states_with_only_outgoing_epsilons
        -- foldl (\t k -> M.delete k t) t2 states_with_only_outgoing_epsilons 

remove_all_unreachable_states :: Transducer -> Transducer
remove_all_unreachable_states  t = 
    let 
        r = reachable (start t) (graph t)
        to_rm = [k | k <- (nodes $ graph t), not $ elem k r]
    in
        -- error $ show $ follow (M.toList t ) [0] 
                -- Transducer a $ foldl (\t k -> M.delete k t) (transitions t) to_rm
        -- error $ "trying to compute reachable for: " ++ show t ++ "reachable" ++ show r
        t{graph = foldl (\g k -> delNode k g) (graph t) to_rm}


{- -- not greatS
follow :: Transducer -> [StateId]
follow t@(Transducer s m) = follow' s m []
follow' :: StateId -> TransducerGraph -> [StateId] -> [StateId]
follow' s g visited = 
    if s `elem` visited then visited else
                let n = concatMap (\case
                        (_, s, Labeled _ )-> [s]
                        (_, s, Call _) -> [s]
                        (_, _, Output _ )-> []
                        ) (labEdges g) in

                if null n then (s:visited) else
                    concatMap (\next -> follow' next g (s:visited)) n -}

remove_epsilon_transitions :: TransducerGraph -> TransducerGraph
remove_epsilon_transitions g = 
    filterLabEdges (\(_, _, l) -> not . is_epsilon $ l) g
    -- M.fromList $ map (\(id,trans) -> (id, filter (not . is_epsilon) trans)) (M.toList t)

update_2 :: StateId -> TransducerGraph -> TransducerGraph
update_2 s g = 
    let cl = epsilon_closure s g
    in 
        insEdgesAndNodes [(s, to, l) | (l, to) <- outgoing cl g] g
        -- M.adjust (\x -> x ++ (outgoing cl t) ) s t

outgoing :: [StateId] -> TransducerGraph -> [(Transition, Int)]
outgoing states g = 
    concatMap (\s -> if gelem s g then
            filter (not . is_epsilon . fst) [ (l, to) | (from, to, l) <- labEdges g, from == s]
        -- filter (not . is_epsilon) trans
        else []) states

is_epsilon :: Transition -> Bool
is_epsilon (Labeled Epsilon) = True
is_epsilon _ = False

filterLabEdges :: (TEdgde -> Bool) -> TransducerGraph -> TransducerGraph
filterLabEdges f g  = 
    mkGraph (labNodes g) (filter f $ labEdges g)

-- TODO
-- This is needed to circumvent a bug in the epsilon elimination procedure 
removeDuplicateEdges :: Transducer -> Transducer
removeDuplicateEdges t = 
    let 
        g = graph t 
        n = labNodes g
        e = labEdges g
    in
        t{graph=mkGraph n (S.toList. S.fromList $ e)}

keepOnlyReachable :: Transducer -> Transducer
keepOnlyReachable t = 
    let 
        r = reachable (start t) (graph t)
        nodes = labNodes (graph t)
        newNodes = filter (\(i, _) -> elem i r) nodes
        -- newEdges = filter (\(_, _, l) -> elem (fst l) r) (labEdges (graph t))
    in
        t{graph = mkGraph newNodes (labEdges (graph t))}

update_output_transitions :: Transducer -> Transducer
-- This function is a bandaid to avoid a bug with output states. To solve this, it would be best to not have ouputs be a label on the edge, but on the node.
update_output_transitions t = 
    let 
        g = graph t
        n = labNodes g
        e = labEdges g
        newE = [case trans of
            (x, y, Output o) -> (x, x, Output o)
            x -> x
            | trans <- e]
    in
        t{graph = mkGraph n newE}