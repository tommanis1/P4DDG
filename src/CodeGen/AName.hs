{- 

Currently, this will only work when 
    - there is no reading of parameters after a recursive call
    - there is at most a single outgoing call for all states
 -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module CodeGen.AName where
import GrammarToTransducer
import Grammar
import Data.Graph.Inductive hiding(size, Context)

{- -- This module implements a adhoc translation from transducers to P4 code 
import qualified Data.Map as M
walk :: 
    StateId -> 
    [(StateId, [Transition])]-> 
    TransducerMap ->
    (String -> String) -> -- TODO
    String
walk 
    start 
    (x:xs) 
    transducermap
    handle_nonterminal
    -- collect a map from stateid to the number of incoming call transition. We need to count this to allocate memory for keeping track of what
    let incomming :: Map Stateid Integer 
    '
largestNumberOfOutgoingCallTransitions :: Tranducer -> Integer
-- We allocate an array where each element records from what call we are taking from multiple alternatives.
largestNumberOfOutgoingCallTransitions t = 

data ReversedTranducer = ReversedTranducer StateId TransducerMap deriving (Show)

swapFromOutgoingToIncoming :: Transducer -> Transducer
swapFromOutgoingToIncoming t = 
    

    t{transitions = new_map, reversed = true} -}

data Context = Context {
    returns :: [(Int, Int)]
    , size :: String
    , indent :: Int
    , gram :: Grammar
    }
empty_context g = Context
    { returns = []
    , size = ""
    , indent = 0
    , gram = g
    }

type Ledge = (Int, Int, Transition)

gen_parser :: () -> Context -> Transducer -> (Context, String)
gen_parser p4context c t =
    let 
        r = returns $ buildReturns p4context c t
        context = c{returns = r}
    in 
    foldl 
        (\(ctx, accStr) s -> 
            let (newCtx, stateStr) = gen_state p4context ctx{indent = indent context} t s 
            in (newCtx, accStr ++ stateStr)
        ) 
        (context, "") 
        (map fst $ labNodes $ graph t)


gen_state :: () -> Context -> Transducer -> Int -> (Context, String)
gen_state p4context context t current_state =
    let (c, body ) = gen_state_body p4context (context{indent = indent context + 1}) t current_state
    in
        (c,indentation (indent context) ++ "state state_" ++ show current_state  ++ "{\n"
            ++ body
        ++ indentation (indent context) ++ "}\n\n")

buildReturns :: () -> Context -> Transducer -> Context
buildReturns p4context context t =
    foldl 
        (\ctx s -> buildReturn p4context ctx t s ) 
        (context) 
        (map fst $ labNodes $ graph t)

-- TODO ugly copy paste
buildReturn :: () -> Context -> Transducer -> Int -> Context
buildReturn p4context context t current_state =
    let o = out (graph t) current_state in
    if single_call_return_pair  o then
        let
            (_, return, Labeled (NonTerminalCall name e)) = fst $ sort_single_call_pair o
            (_, next, Call _) = snd $ sort_single_call_pair o
            id = next_id $ returns context
            new_returns = (returns context) ++ [(id, return)]

        in
            context{returns = new_returns}
    else context

gen_state_body :: () -> Context -> Transducer -> Int -> (Context, String)
gen_state_body p4context context t current_state =
    let o = out (graph t) current_state
    in

    if singleTerminal  o then
        let [(current_state, target, Labeled (Terminal terminalName))] = o in
            (context, i ++ "pseudo_instruction.extract("++ terminalName ++");\n" ++ i ++ "transition " ++ show (mkStateName target) ++ ";")

    else if accepting_state current_state then
        (context,
        i ++  size context ++ " tmp_return = " ++ return_stack_name ++ ".pop();\n"
        ++ i ++ "transition select (tmp_return) {\n"
            ++ concatMap (\x -> i_plus1 ++ x ++ "\n") (build_return_trans_for_accepting_states (returns context))
        ++ i ++ "}\n")


    else if single_call_return_pair  o then
        let
            (_, return, Labeled (NonTerminalCall name e)) = fst $ sort_single_call_pair o
            (_, next, Call _) = snd $ sort_single_call_pair o
            id = next_id $ returns context
            -- new_returns = (returns context) ++ [(id, return)]

        in
            (context,
                -- context{returns = new_returns},
                   
                   i ++ return_stack_name ++ ".push("++show id++");\n"
                ++ i ++ (findVar context name) ++ " = " ++ show e ++ ";\n" 
                ++ i ++ "transition " ++ mkStateName next ++ ";")

    else if all is_constraint (labs o) then (context,
           i ++ "bit<" ++ show (number_of_bits_needed_to_hold (length o)) ++ "> tmp;\n"
        ++ (concatMap ((++) i) $ map gen_constraint (zip [0..] o))
        ++ i ++ "transition select (tmp) {\n"
            ++ (concatMap ((++) i_plus1) (map gen_constraint_transition (zip [0..] o)))
        ++ i ++ "}\n")


    else
        error $ "gen - non implemented yet " ++ show o


    -- case o of 
    --     [(current_state, target, Labeled Nonterminal nonterminalName)] -> "pseudo_instruction(extract) name;\ntransition " ++ show mkStateName target ++ ";"
    --     _ -> error $ "gen - non implemented yet " ++ show o

    where
        gen_constraint :: (Int, Ledge) -> String
        gen_constraint (i, (_, _, Labeled (Constraint c))) =
            "if (" ++ c ++ "){" ++ "tmp = " ++ show i ++ ";}\n"
        gen_constraint _ = error "gen_constraint on something other than a constraint"

        gen_constraint_transition (i, (_, target, _)) =
            show i ++ " : " ++ mkStateName target ++ ";\n"
        i = indentation (indent context)
        i_plus1 = indentation (1 + indent context)


        singleTerminal :: [Ledge] -> Bool
        singleTerminal [(_, _, Labeled (Terminal _))] = True
        singleTerminal _ = False

        accepting_state :: Int -> Bool
        accepting_state s = not . null $ [ () | (_, _, Output _) <- out (graph t) s]

        is_constraint :: Transition -> Bool
        is_constraint ( Labeled (Constraint _)) = True
        is_constraint _ = False

        return_stack_name = "return_stack"

single_call_return_pair :: [Ledge] -> Bool
single_call_return_pair [(_, _, Labeled (NonTerminalCall _ _)), (_, _, Call _)] = True
single_call_return_pair [(_, _, Call _), (_, _, Labeled (NonTerminalCall _ _))] = True
single_call_return_pair _ = False

sort_single_call_pair :: [Ledge] -> (Ledge, Ledge)
sort_single_call_pair [f@(_, _, Labeled (NonTerminalCall _ _)), s@(_, _, Call _)] = (f,s)
sort_single_call_pair [s@(_, _, Call _), f@(_, _, Labeled (NonTerminalCall _ _))] = (f,s)


        --TODO
number_of_bits_needed_to_hold :: Int -> Int
number_of_bits_needed_to_hold _ = 10

build_return_trans_for_accepting_states :: [(Int, Int)] ->  [String]
-- TODO, show id
build_return_trans_for_accepting_states = map (\(id,return_state) -> show id ++ " : " ++ mkStateName return_state ++ ";" )

mkStateName :: Int -> String
mkStateName s = "state_" ++ show s

duplicate str n = [1..n] >> str
indentation :: Int -> String
indentation x  = duplicate " " (4 * x)

next_id :: [(Int, Int)] -> Int
next_id [] = 0
next_id x = 1 + maximum (map fst x)

findVar :: Context -> String -> String
findVar context nonTerminalName = 
    let 
        g = gram context
        (Nonterminal _ params _) = head [e | e@(Nonterminal name _ _) <- g, name == nonTerminalName]
    in 
        head params

{- 
state state_0 :
    packet.extract ethernet 
    transition state_6
-}

labs :: [(Int, Int, Transition)] -> [Transition]
labs y = (map (\(_, _, x) -> x) y)