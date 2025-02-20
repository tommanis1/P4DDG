{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module GrammarToTransducer where

import Grammar

import Control.Monad.State.Lazy
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)


type TransducerMap = (M.Map StateId [Transition])
data  Transducer = Transducer StateId TransducerMap 
    deriving (Show)
get_map :: Transducer -> TransducerMap
get_map (Transducer _ x ) = x

data Transition = 
    Labeled Label StateId
    | Output NonTerminalId
    | Call Expression StateId deriving (Show)

type StateId = Integer
type NonTerminalMap = M.Map NonTerminalId StateId

type UniqueId = State Integer


u = M.unionWith (++)

rule_to_transducer :: NonTerminalMap -> Rule -> UniqueId (StateId, StateId, TransducerMap)
rule_to_transducer s_map (RuleLabel l) = rule_to_transducer_label s_map l
rule_to_transducer s_map (Sequence r1 r2) = do
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    (s2, f2, t2) <- rule_to_transducer s_map r2

    let t3 = M.fromList [(s, [Labeled Epsilon s1]), (f1, [Labeled Epsilon s2]), (f2, [Labeled Epsilon f])]

    return (s, f, u (u t1 t2) t3 )


rule_to_transducer s_map (Alternation r1 r2) = do
    (s,f) <- fresh_2
    (s1, f1, t1) <- rule_to_transducer s_map r1 
    (s2, f2, t2) <- rule_to_transducer s_map r2

    let t3 = M.fromList [(s, [Labeled Epsilon s1, Labeled Epsilon s2]), (f1, [Labeled Epsilon f]), (f2, [Labeled Epsilon f])]

    return (s, f, u (u t1 t2) t3 )

rule_to_transducer_label :: NonTerminalMap -> Label -> UniqueId (StateId, StateId, TransducerMap)
rule_to_transducer_label s_map l@(NonTerminalCall name e) = do
    (s,f) <- fresh_2
    let m_sa = M.lookup name s_map 
    case m_sa of 
        Just sa -> 
            return (s, f, M.fromList [(s, [Labeled l f, Call e sa])])
        Nothing ->
            error "Error: rule_to_transducer_label"
rule_to_transducer_label s Empty = do
    (s,f) <- fresh_2
    return (s, f, M.fromList [(s, []), (f, [])])
rule_to_transducer_label s l = do
    (s,f) <- fresh_2
    return (s, f, M.fromList [(s, [Labeled l f])])

fresh :: UniqueId Integer
fresh = do
    lastID <- get
    put (lastID + 1)
    return lastID

fresh_2 :: UniqueId (Integer, Integer)
fresh_2 = do
    a <- fresh
    b <- fresh
    return (a,b)


grammar_to_transducer' :: Grammar -> UniqueId Transducer
grammar_to_transducer' grammar = do
    let s =  [(name , state_id)| ((Nonterminal name _ _), state_id) <- zip grammar [0..]]
    let s_map :: M.Map NonTerminalId Integer = M.fromList s
    put (1 + (fromIntegral $ length grammar) )
    
    t_n :: [(Integer, Integer, TransducerMap)] <- sequence $ map (rule_to_transducer s_map) $ map (\(Nonterminal _ _ r) -> r) grammar

    -- assert equal length between s and t_n
    -- T_init = [s_A0 −→ s_0]; · · · ; [s_Ak −→ s_k]


    let t_init_union_final :: TransducerMap  =  M.fromList $ concat [ 
            [(state_id, [Labeled Epsilon s_i]), (f_i, [Output a_i])  ]
                | ((a_i, state_id), (s_i, f_i, _)) <- zip s t_n]


    let t_n_ :: TransducerMap = foldr u M.empty [ t | (s, f, t) <- t_n]
    return $ Transducer (snd $ head s) (u t_init_union_final t_n_)
grammar_to_transducer :: Grammar -> Transducer
grammar_to_transducer g = evalState (grammar_to_transducer' g) 0

epsilon_closure :: StateId -> TransducerMap -> [StateId]
epsilon_closure s t = 
    filter (\x -> x /= s) $ S.toList . S.fromList $ epsilon_closure' s t []

epsilon_closure' :: StateId -> TransducerMap -> [StateId] -> [StateId]
epsilon_closure' s t visited = 
    case M.lookup s t of
            (Just transitions) -> 
                let e = [target | Labeled Epsilon target <- transitions, target `notElem` visited] in
                if null e then (s:visited) else
                    concatMap (\next -> epsilon_closure' next t (s:visited)) e
            Nothing -> [] 

update_1 :: StateId -> TransducerMap -> TransducerMap
update_1 s t = 
    let cl = epsilon_closure s t
        accepting = accepting_states cl t
    in 
        if null accepting then t
        else 
            -- make s an accepting state
            t `u` (M.fromList $ map (\(_, transitions ) -> (s, transitions)) (M.toList accepting))

accepting_states :: [StateId] -> TransducerMap -> TransducerMap
accepting_states states t = 
    foldl u M.empty $ map (outputs t) states

outputs :: TransducerMap -> StateId -> TransducerMap
outputs t s = case M.lookup s t of
    Just trans -> M.fromList [(s, [Output target | Output target <- trans])]
    Nothing -> M.empty

epsilon_elimination :: Transducer -> Transducer
epsilon_elimination (Transducer s m) = 
    -- https://web.cecs.pdx.edu/~sheard/course/CS581/notes/NfaEpsilonDefined.pdf
    let 
        -- states_with_only_outgoing_epsilons = filter (\x -> null $ (outgoing [x] t)) (M.keys t)
    -- step one: mark all states that can reach an accepting state trough epsilon-transitions as accepting
        t1 = foldl (\x y -> update_1 y x) (m) (M.keys $ m)
        -- Add an arc from p to q labeled a iff there is an arc labeled a in N from
        -- some state in ECLOSE(p) to q.
        t2 = foldl (\x y -> update_2 y x) (t1) (M.keys t1)
    in
        -- remove_all_unreachable_states . 
        remove_all_unreachable_states $ 
        (Transducer s $ remove_epsilon_transitions $ t2)
        -- error $ "AAAAAAA" ++ show states_with_only_outgoing_epsilons
        -- foldl (\t k -> M.delete k t) t2 states_with_only_outgoing_epsilons 

remove_all_unreachable_states :: Transducer -> Transducer
remove_all_unreachable_states  t@(Transducer a _) =
    let 
        reachable = follow t
        to_rm = [k | k <- M.keys (get_map t), not $ elem k reachable]
    in
        -- error $ show $ follow (M.toList t ) [0] 
        Transducer a $ foldl (\t k -> M.delete k t) (get_map t) to_rm

-- not greatS
follow :: Transducer -> [StateId]
follow t@(Transducer s m) = follow' s m []
follow' :: StateId -> TransducerMap -> [StateId] -> [StateId]
follow' s t visited = 
    if s `elem` visited then visited else
    case M.lookup s t of
            (Just transitions) -> 
                let n = concatMap (\case
                        Labeled _ s -> [s]
                        Call _ s -> [s]
                        Output _ -> []
                        ) transitions in

                if null n then (s:visited) else
                    concatMap (\next -> follow' next t (s:visited)) n
            Nothing -> []  

remove_epsilon_transitions :: TransducerMap -> TransducerMap
remove_epsilon_transitions t = 
    M.fromList $ map (\(id,trans) -> (id, filter (not . is_epsilon) trans)) (M.toList t)

update_2 :: StateId -> TransducerMap -> TransducerMap
update_2 s t = 
    let cl = epsilon_closure s t
    in 
        M.adjust (\x -> x ++ (outgoing cl t) ) s t

outgoing :: [StateId] -> TransducerMap -> [Transition]
outgoing states t = 
    concatMap (\s -> case M.lookup s (t) of
        Just trans -> filter (not . is_epsilon) trans
        Nothing -> []) states

is_epsilon :: Transition -> Bool
is_epsilon (Labeled Epsilon _) = True
is_epsilon _ = False