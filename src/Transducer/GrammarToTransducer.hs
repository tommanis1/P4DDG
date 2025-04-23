{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, TypeSynonymInstances , FlexibleInstances#-}

module Transducer.GrammarToTransducer where

import DDG.Types

import Control.Monad.State.Lazy
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS
import Data.List
import P4Types
import DDG.P4DDG
import qualified Transducer.Def as T
import Transducer.Def (Transition(Return))
import Data.Type.Coercion (trans)

type TransducerGraph e = Gr T.StateLabel (T.Transition e)

type StateId = Int
type NonTerminalMap = M.Map NonTerminalId StateId

type UniqueId = State Int


u :: TransducerGraph e -> TransducerGraph e -> TransducerGraph e
u g1 g2 = 
    let 
        all_nodes = labNodes g1 ++ labNodes g2
        all_edges = labEdges g1 ++ labEdges g2
        -- unique_ids = nub $ map fst all_nodes
        outputs = [(i, T.Output o) | (i, T.Output o) <- all_nodes]
        rest = [(i, T.NoLabel) | (i, T.NoLabel) <- all_nodes, i `notElem` (nub $ map fst outputs)]
        -- n1 = labNodes g1
        -- n2 = labNodes g2

        -- e1 = labEdges g1
        -- e2 = labEdges g2
    in
        mkGraph (outputs ++ rest) all_edges

{- insEdgesAndNodes :: [(Int, Int, T.Transition e)] -> TransducerGraph e -> TransducerGraph e
insEdgesAndNodes new g = 
        g `u` mkGraph nodes new

    where 
        nodes = S.toList . S.fromList $ concatMap (\(s1,s2, _) -> [(s1, T.NoLabel), (s2, T.NoLabel)]) new
 -}
insEdgesAndNodes :: [(Int, Int, T.Transition e)] -> TransducerGraph e -> TransducerGraph e
insEdgesAndNodes new g = 
    let
        node_ids = map fst $ labNodes g 
        all_new_nodes = S.toList . S.fromList $ concatMap (\(s1,s2, _) -> [(s1, T.NoLabel), (s2, T.NoLabel)]) new
        new_nodes = [ x | x@(s,_ ) <- all_new_nodes, s `notElem` node_ids]
    in mkGraph (labNodes g ++ new_nodes) (labEdges g ++ new)

rule_to_transducer :: (Show e) => NonTerminalMap -> Rule e -> UniqueId (StateId, StateId, TransducerGraph e)
rule_to_transducer s_map (DDG.Types.Label l) = rule_to_transducer_label s_map l
rule_to_transducer s_map (DDG.Types.Sequence r1 r2) = do
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    (s2, f2, t2) <- rule_to_transducer s_map r2

    -- let t3 = M.fromList [(s, [Labeled Epsilon s1]), (f1, [Labeled Epsilon s2]), (f2, [Labeled Epsilon f])]
    let t3 = insEdgesAndNodes [
                (s, s1,  T.Labeled Epsilon)
                , (f1, s2,  T.Labeled Epsilon)
                , (f2, f,  T.Labeled Epsilon)
            ] empty

    return (s, f, t1 `u` (t2 `u` t3))



rule_to_transducer s_map (Alternation r1 r2) = do
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    (s2, f2, t2) <- rule_to_transducer s_map r2

    let t3 = insEdgesAndNodes [
                (s, s2,  T.Labeled Epsilon)
                , (s, s1,  T.Labeled Epsilon)
                , (f1, f,  T.Labeled Epsilon)
                , (f2, f,  T.Labeled Epsilon)
            ] empty

    -- let t3 = M.fromList [(s, [Labeled Epsilon s1, Labeled Epsilon s2]), (f1, [Labeled Epsilon f]), (f2, [Labeled Epsilon f])]

    return (s, f, t1 `u` (t2 `u` t3))

rule_to_transducer s_map (KleineClosure r1) = do 
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    let t3 = insEdgesAndNodes [
                (s, s1,  T.Labeled Epsilon)
                , (f1, f,  T.Labeled Epsilon)
                , (s, f,  T.Labeled Epsilon)
                , (f1, s1,  T.Labeled Epsilon)
            ] empty
    return (s, f, t1 `u` t3)


rule_to_transducer_label :: NonTerminalMap -> Label e -> UniqueId (StateId, StateId, TransducerGraph e)
rule_to_transducer_label s_map l@(NonTerminalCall name e) = do
    (s,f) <- fresh_2
    let m_sa = M.lookup name s_map 
    case m_sa of 
        Just sa -> 
            return (s, f, insEdgesAndNodes [(s, f, T.Return name e),(s, sa,  T.Labeled l )] empty) -- Here
        Nothing ->
            error $ "Error: rule_to_transducer_label\n can't find:" ++ show name
rule_to_transducer_label s Empty = do
    (s,f) <- fresh_2
    return (s, f, mkGraph [(s, T.NoLabel),(f, T.NoLabel)] [])
rule_to_transducer_label s l = do
    (s,f) <- fresh_2
    return (s, f, insEdgesAndNodes [(s,f, T.Labeled l)] empty)

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


grammar_to_transducer' :: Grammar -> UniqueId T.P4Transducer
grammar_to_transducer' grammar = do
    let s =  [(name , state_id)| ((Nonterminal name _ _), state_id) <- zip grammar [0..]]
    let s_map :: M.Map NonTerminalId Int = M.fromList s
    put (1 + length grammar )
    
    t_n :: [(Int, Int, TransducerGraph e)] <- sequence $ map (rule_to_transducer s_map) $ map (\(Nonterminal _ _ r) -> r) grammar

    -- assert equal length between s and t_n
    -- T_init = [s_A0 −→ s_0]; · · · ; [s_Ak −→ s_k]

    let t_init_union_final :: TransducerGraph e  =  
            -- let e = concat [ 
            --         [(state_id, s_i, T.Labeled Epsilon) ]
            --             | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]
            --     n = [(f_i, T.Output a_i) | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]
            --     in mkGraph n e
            insEdgesAndNodes (concat [ 
                [(state_id, s_i, T.Labeled Epsilon) ]
                    | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]) 
                    (mkGraph  [(f_i, T.Output a_i) | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]
                     [])
        
        -- M.fromList $ concat [ 
        --     [(state_id, [Labeled Epsilon s_i]), (f_i, [Output a_i])  ]
        --         | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]


    let t_n_ :: TransducerGraph e =  foldr ((u)) empty [ t | (s, f, t) <- t_n]

    return $ T.Transducer{ T.start = snd $ head s, T.graph = (t_init_union_final `u` t_n_)}
grammar_to_transducer :: Grammar -> T.P4Transducer
grammar_to_transducer g = evalState (grammar_to_transducer' g) 0

epsilon_closure :: StateId -> TransducerGraph e -> [StateId]
epsilon_closure s t = 
    filter (\x -> x /= s) $ S.toList . S.fromList $ epsilon_closure' s t []

epsilon_closure' :: StateId -> TransducerGraph e -> [StateId] -> [StateId]
epsilon_closure' s t visited = 
                let e = [target | (x, target,  T.Labeled Epsilon) <- labEdges t, target `notElem` visited, x == s] in
                if null e then (s:visited) else
                    concatMap (\next -> epsilon_closure' next t (s:visited)) e


update_1 :: (StateId, T.StateLabel) -> TransducerGraph e -> TransducerGraph e
update_1 (s, l) t = 
    let cl = epsilon_closure s t
        accepting ::  [(Int, T.StateLabel)] = [ x | x@(_, T.Output _) <- labNodes t]
    in 
        if null accepting then t
        else 
            let 
                nodes = [ x | x <- labNodes t, x /= (s, T.NoLabel) ]
                edges = labEdges t
            in
                mkGraph (nodes ++ [(s, T.Output o) | (to, T.Output o) <- accepting]) edges
            -- insEdgesAndNodes [(s, T.Output o) | (to, T.Output o) <- accepting] t
            -- make s an accepting state
            -- t `u` (M.fromList $ map (\(_, transitions ) -> (s, transitions)) (M.toList accepting))

{- accepting_states :: [StateId] -> TransducerGraph e -> [(Int, T.StateLabel)]
accepting_states states t = 

    concatMap (outputs t) states

outputs :: TransducerGraph e -> StateId ->[(Int, T.StateLabel)]
outputs t s = 
    [ (to, T.Output o) | x@(from, T.Output o) <- labEdges t, from == s] 
    -- case M.lookup s t of
    -- Just trans -> M.fromList [(s, [Output target | Output target <- trans])]
    -- Nothing -> M.empty -}

epsilon_elimination :: T.P4Transducer -> T.P4Transducer
epsilon_elimination t = 
    -- https://web.cecs.pdx.edu/~sheard/course/CS581/notes/NfaEpsilonDefined.pdf
    let 
        -- states_with_only_outgoing_epsilons = filter (\x -> null $ (outgoing [x] t)) (M.keys t)
    -- step one: mark all states that can reach an accepting state trough epsilon-transitions as accepting
        g1 = foldl (\x y -> update_1 y x) (T.graph t) (labNodes $ T.graph t)
        -- Add an arc from p to q labeled a iff there is an arc labeled a in N from
        -- some state in ECLOSE(p) to q.
        g2 = foldl (\x y -> update_2 y x) (g1) (labNodes g1)
    in
        -- remove_all_unreachable_states (t{graph=g1})

        remove_all_unreachable_states $ 
        t{T.graph = remove_epsilon_transitions g2}
        -- t{T.graph =  g2}
        -- (Transducer s $ remove_epsilon_transitions $ t2)
        -- error $ "AAAAAAA" ++ show states_with_only_outgoing_epsilons
        -- foldl (\t k -> M.delete k t) t2 states_with_only_outgoing_epsilons 

remove_all_unreachable_states :: T.Transducer a b -> T.Transducer a b
remove_all_unreachable_states  t = 
    let 
        r = reachable (T.start t) (T.graph t)
        to_rm = [k | k <- map fst (labNodes $ T.graph t), not $ elem k r]
    in
        -- error $ show $ follow (M.toList t ) [0] 
                -- Transducer a $ foldl (\t k -> M.delete k t) (transitions t) to_rm
        -- error $ "trying to compute reachable for: " ++ show t ++ "reachable" ++ show r
        t{T.graph = foldl (\g k -> delNode k g) (T.graph t) to_rm}


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

remove_epsilon_transitions :: TransducerGraph e -> TransducerGraph e
remove_epsilon_transitions g = 
    mkGraph (labNodes g) (filter (\(_, _, l) -> not . is_epsilon $ l) $ labEdges g)
    -- M.fromList $ map (\(id,trans) -> (id, filter (not . is_epsilon) trans)) (M.toList t)

update_2 :: (StateId, T.StateLabel) -> TransducerGraph e -> TransducerGraph e
update_2 (s, label) g = 
    let cl = epsilon_closure s g
    in 
        insEdgesAndNodes [(s, to, l) | (l, to) <- outgoing cl g] g
        -- M.adjust (\x -> x ++ (outgoing cl t) ) s t

outgoing :: [StateId] -> TransducerGraph e -> [(T.Transition e, Int)]
outgoing states g = 
    concatMap (\s -> if gelem s g then
            filter (not . is_epsilon . fst) [ (l, to) | (from, to, l) <- labEdges g, from == s]
        -- filter (not . is_epsilon) trans
        else []) states

is_epsilon :: T.Transition e -> Bool
is_epsilon (T.Labeled Epsilon) = True
is_epsilon _ = False

-- rm
{- filterLabEdges :: (LEdge -> Bool) -> TransducerGraph a b -> TransducerGraph a b
filterLabEdges f g  = 
    mkGraph (labNodes g) (filter f $ labEdges g) -}

-- TODO
-- This is needed to circumvent a bug in the epsilon elimination procedure 
removeDuplicateEdges :: (Ord b) => T.Transducer a b -> T.Transducer a b
removeDuplicateEdges t = 
    let 
        g =T.graph t 
        n = labNodes g
        e = labEdges g
    in
        t{T.graph=mkGraph n (S.toList. S.fromList $ e)}

keepOnlyReachable :: T.Transducer a b -> T.Transducer a b
keepOnlyReachable t = 
    let 
        r = reachable (T.start t) (T.graph t)
        nodes = labNodes (T.graph t)
        newNodes = filter (\(i, _) -> elem i r) nodes
        -- newEdges = filter (\(_, _, l) -> elem (fst l) r) (labEdges (graph t))
    in
        t{T.graph = mkGraph newNodes (labEdges (T.graph t))}

{- update_output_transitions :: T.Transducer -> T.Transducer
-- This function is a bandaid to avoid a bug with output states. To solve this, it would be best to not have ouputs be a label on the edge, but on the node.
update_output_transitions t = 
    let 
        g = T.graph t
        n = labNodes g
        e = labEdges g
        newE = [case trans of
            (x, y, Output o) -> (x, x, Output o)
            x -> x
            | trans <- e]
    in
        t{T.graph = mkGraph n newE} -}

moveElseTransitions :: T.P4Transducer -> T.P4Transducer
-- This function inserts an epsilon transition in case a state has guarded and non-guarded transitions
-- The effect of this is that if a state has one or more guarded transitions than the only transitions are epsilon or guarded transitions

moveElseTransitions t = 
    let 
        g = T.graph t
        states = map fst $ labNodes g
        mixedStates = filter (hasGuardedAndNonGuarded g) states
        -- Create new graph with modifications
        newGraph = foldl modifyTransitions g mixedStates
    in
        t{T.graph = newGraph}
    where
        -- Check if a state has both guarded and non-guarded transitions
        hasGuardedAndNonGuarded :: TransducerGraph e -> Int -> Bool
        hasGuardedAndNonGuarded g state = 
            let outEdges = out g state
                hasGuarded = any isGuardedTransition outEdges
                hasNonGuarded = any (not . isGuardedTransition) outEdges
            in hasGuarded && hasNonGuarded
            
        -- Determine if a transition is guarded
        isGuardedTransition :: (Int, Int, T.Transition e) -> Bool
        isGuardedTransition (_, _, (T.Labeled (Constraint _))) = True
        isGuardedTransition _ = False
        isCallOrReturn :: (Int, Int, T.Transition e) -> Bool
        isCallOrReturn (_, _, (T.Labeled (NonTerminalCall _ _))) = True
        isCallOrReturn (_, _, (Return _ _)) = True
        isCallOrReturn _ = False

        -- Modify transitions for a state with mixed transitions
        modifyTransitions :: (Eq e) => TransducerGraph e -> Int -> TransducerGraph e
        modifyTransitions graph state = 
            let 
                outEdges = out graph state
                (guarded, nonGuarded) = partition isGuardedTransition outEdges
                (calls, nonGuardedNotCalls) = partition isCallOrReturn nonGuarded
                edges = [edge | edge <- labEdges graph , edge `notElem` nonGuarded] -- This is a big search for no good reason

                new = zip nonGuardedNotCalls [(newIdGraph graph) ..]
                graph_with_transitions_removed = mkGraph (labNodes graph) edges
                newGraph = foldl (insertTransition state ) graph_with_transitions_removed new

            in 
                if length calls > 2 then error ""
                else 
                    let 
                        i = newIdGraph newGraph
                        n = labNodes newGraph ++ [(i, T.NoLabel)]
                        e = labEdges newGraph ++ [ (i, to, l) | (from, to, l) <- calls] ++ [(state, i, T.Labeled Epsilon)]
                    in 
                        mkGraph n e

        insertTransition :: Int -> TransducerGraph e -> ((Int, Int, T.Transition e), Int) -> TransducerGraph e
        insertTransition og graph ((from, to, transition), id) = 
            let 
                -- insert state id
                n = labNodes graph ++ [(id, T.NoLabel)]
                e = labEdges graph ++ [(og, id, (T.Labeled Epsilon)), (id, to, transition)] in
            mkGraph n e

newIdGraph :: TransducerGraph e -> Int
newIdGraph g = 
    let
        nodes = labNodes g
        maxId = maximum $ map fst nodes
    in
        maxId + 1
-- If the next state after an epsilon has a single outgoing transition, then we can always merge 
-- this removes the otherwise epsilon transition, so we'd have to add it back 

-- I a state has single incoming epsilon transition, then we can merge this state with the previous one


-- merge :: (Int, Int, T.Transition e) -> TransducerGraph e -> TransducerGraph e
-- merge (from, to, transition) g = 
--     let
--         out_from = outE g from
--         in_to = inE g to
--     in 

-- combineStatesWithEpsilon :: T.P4Transducer -> T.P4Transducer
-- combineStatesWithEpsilon t = 
--     let
--         g = T.graph t
--         epsilons = [x | x@(from, to, T.Labeled Epsilon) <- labEdges g]
--     in 
--         t{T.graph =foldl (\g' (from, to, transition) -> combineStates (from, to, transition) g')  g epsilons}
-- combineStates :: (Eq e) => (Int, Int, T.Transition e) -> TransducerGraph e -> TransducerGraph e
-- combineStates (source, target, T.Labeled(Epsilon)) graph =
--     let 
--         edges = [ e | e<- labEdges graph, e /= (source, target, T.Labeled Epsilon)]
--         nodes = labNodes graph
--     in 
--         mkGraph nodes (map updateSingle edges)

--     where 
--         updateSingle :: (Int, Int, T.Transition e) -> (Int, Int, T.Transition e)
--         updateSingle (x, y, z) =
--             if x == target then 
--                 (source, y, z)
--             else
--                 (x, y, z)
-- combineStates _ graph = graph
combineStatesWithEpsilon :: T.P4Transducer -> T.P4Transducer
-- This searches for epsilon transitions from s1 to s2 and removes s2, if this is the only edge to s2 
combineStatesWithEpsilon t = 
    let
        g = T.graph t
        eps = [x | x@(from, to, T.Labeled Epsilon) <- labEdges g]
        output_ids = [i | (i, T.Output _) <- labNodes g]
        epsilons = [x | x@(from, to, T.Labeled Epsilon) <- labEdges g, 
                        to `notElem` output_ids, singleincoming to g] -- This assumes that only output states dont have any outgoing transition, which is true for valid grammars 
    in 
        if null epsilons then t
        else
            combineStatesWithEpsilon t{T.graph = combineStates (head epsilons) g}
        -- t{T.graph =foldl (\g' (from, to, transition) -> combineStates (from, to, transition) g')  g epsilons}
combineStates :: (Eq e) => (Int, Int, T.Transition e) -> TransducerGraph e -> TransducerGraph e
combineStates (source, target, T.Labeled Epsilon) graph =
    let 
        edges = [ e | e<- labEdges graph, e /= (source, target, T.Labeled Epsilon)]
        new_edges = (map updateSingle edges)
    in 
        -- if edges == new_edges then graph
        -- else
            mkGraph (labNodes graph) (map updateSingle edges)

    where 
        updateSingle :: (Int, Int, T.Transition e) -> (Int, Int, T.Transition e)
        updateSingle (x, y, z) =
            if x == target then 
                (source, y, z)
            else
                (x, y, z)
combineStates _ graph = graph

singleincoming state g = 
    let incoming =  [ e | e@(from, to, _)<- labEdges g, to == state]
    in length incoming ==1 


jump :: T.P4Transducer -> T.P4Transducer
jump t = 
    let g = T.graph t
        statesWithOneEpsilonOut = 
            [(s, to) | (s,_) <- labNodes g, singletonEpsilon (outgoing s), (_, to, T.Labeled Epsilon) <-(outgoing s)  ]
        new_graph = foldl (\g' (s, to) -> update (s, to) g')  g statesWithOneEpsilonOut
    in 
        t{T.graph=new_graph}


    where
        outgoing s =  [ e | e@(from, to, _)<- labEdges (T.graph t), from == s]
        singletonEpsilon [(_, _, T.Labeled Epsilon)] = True
        singletonEpsilon _= False

update (s, to) g = 
    mkGraph (labNodes g) (map (\(s1, s2, l)-> if s2==s then (s1, to, l) else (s1, s2, l)) (labEdges g))
-- statesWithOneIncomingAndOneEpisilonOut = 
    -- [s | (s,_) <- labNodes g, length (incoming s) ==1, singletonEpsilon (outgoing s) ]
    -- where
    --     incoming s =  [ e | e@(from, to, _)<- labEdges g, to == s]
    --     outgoing s =  [ e | e@(from, to, _)<- labEdges g, from == s]
    --     singletonEpsilon [(_, _, T.Labeled Epsilon)] = True
    --     singletonEpsilon _= False