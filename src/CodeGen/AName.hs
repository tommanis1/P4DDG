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
import Data.List (intercalate)


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

gen_p4 :: Transducer -> String
gen_p4 = error "gen_p4 not implemented"

gen_parser :: () -> Context -> Transducer -> (Context, String)
gen_parser p4context c t =
    
    let 
        (con, parser) = gen_parser' p4context c t
        initial_transition = (indentation $indent con) ++ "state start {transition state_0;}\n"
    in 
        (con, (header_stack_decl p4context con) ++ "\n\n" ++ initial_transition ++ parser)

gen_parser' :: () -> Context -> Transducer -> (Context, String)
gen_parser' p4context c t =
    let 
        
        r = returns $ buildReturns p4context c t
        context = c{returns = r}
        param_decls = gen_decls_for_parameterized_nonterminal context
        (new_c,code) = 
                foldl 
            (\(ctx, accStr) s -> 
                let (newCtx, stateStr) = gen_state p4context ctx{indent = indent context} t s 
                in (newCtx, accStr ++ stateStr)
            ) 
            (context, "") 
            (map fst $ labNodes $ graph t)

    in 
        (new_c, param_decls ++ code)

gen_decls_for_parameterized_nonterminal :: Context -> String
gen_decls_for_parameterized_nonterminal c = 
    let 
        g = gram c
        params = find_params g []
        i = indentation $ indent c
    in 
       (intercalate "\n" $ map (\(Param t id) -> i ++ t ++ " " ++ id ++ ";") params) ++ "\n"

    where 
        find_params :: Grammar -> [Param] -> [Param]
        -- find_params _ _ = []
        find_params [] p = p
        find_params ((Nonterminal _ params _):xs) p = find_params xs (p ++ params)

gen_state :: () -> Context -> Transducer -> Int -> (Context, String)
gen_state p4context context t current_state =
    let (c, body ) = gen_state_body p4context (context{indent = indent context + 1}) t current_state
    in
        (c,indentation (indent context) ++ "state state_" ++ show current_state  ++ " {\n"
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
            (context, i ++ "packet.extract("++ terminalName ++");\n" ++ i ++ "transition " ++ (mkStateName target) ++ ";")

    else if accepting_state current_state then
        (context,
        -- i ++  size context ++ " tmp_return = " ++ return_stack_name ++ ".pop();\n"
        --    i ++ "bit<" ++ size context ++"> tmp_return = " ++ return_stack_name ++ "[" ++ return_stack_index ++ "].val;\n"
           i ++ "// accepting state\n"
           ++ i ++ "int<16> tmp_return;\n"
           ++ i ++ "if (return_stack_index == 0) { tmp_return = 0;} else {\n"
           ++ i ++ "    "  ++ "tmp_return = " ++ return_stack_name ++ "[" ++ return_stack_index ++ "].val;" ++ return_stack_index ++ " = " ++ return_stack_index ++ " - 1;" ++"}\n"

        -- ++ i ++ return_stack_index ++ " = " ++ return_stack_index ++ " - 1;"
        ++ i ++ "transition select (tmp_return) {\n"
            ++ i ++ "0 : accept;\n"
            ++ concatMap (\x -> i_plus1 ++ x ++ "\n") (build_return_trans_for_accepting_states (returns context))
        ++ i ++ "}\n")


    else if single_call_return_pair  o then
        let
            (_, return, Labeled (NonTerminalCall name e)) = fst $ sort_single_call_pair o
            (_, next, Call _) = snd $ sort_single_call_pair o
            id = (next_id $ returns context) - 2 -- TODO what is going wrong here 
            -- new_returns = (returns context) ++ [(id, return)]

        in
            (context,
                -- context{returns = new_returns},
                   
                --    i ++ return_stack_name ++ ".push("++show id++");\n"

                --    i ++ "if (return_stack.size == ) "
                   i ++ "return_stack_index = return_stack_index + 1;\n"
                ++ i ++ "return_stack[return_stack_index].val = " ++ show id ++";\n"
                ++ i ++ (findVar context name) ++ " = " ++ e ++ ";\n" 
                ++ i ++ "transition " ++ mkStateName next ++ ";")

    else if all is_constraint (labs o) then (context,
           i ++ "bit<" ++ show (number_of_bits_needed_to_hold (length o)) ++ "> tmp;\n"
        ++ (concatMap ((++) i) $ map gen_constraint (zip [0..] o))
        ++ i ++ "transition select (tmp) {\n"
            ++ (concatMap ((++) i_plus1) (map gen_constraint_transition (zip [0..] o)))
        ++ i ++ "}\n")

    else if length (o) == 1 && all is_Bindings (labs o) then 
        let 
            (_, target, Labeled (Bindings l)) = ( o) !! 0
        in 
           (context, ((intercalate "\n" (map ((++) i) $ map show_Binding l))
           ++ i ++ "transition state_" ++ show target ++ ";"))



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

        is_Bindings :: Transition -> Bool
        is_Bindings ( Labeled(Bindings _)) = True
        is_Bindings _ = False
        return_stack_name = "return_stack"

        show_Binding :: (String, String, String) -> String 
        show_Binding (t, id, e) = id ++ " = " ++ e ++ ";"

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
next_id [] = 1
next_id x = 1 + maximum (map fst x)

findVar :: Context -> String -> String
findVar context nonTerminalName = 
    let 
        g = gram context
        (Nonterminal _ params _) = head [e | e@(Nonterminal name _ _) <- g, name == nonTerminalName]
        (Param _ name ) = head params
    in 
        name

{- 
state state_0 :
    packet.extract ethernet 
    transition state_6
-}

labs :: [(Int, Int, Transition)] -> [Transition]
labs y = (map (\(_, _, x) -> x) y)

header_stack_decl :: () -> Context -> String
header_stack_decl _ c = 
    let s = integer_to_n_bits_needed_to_hold_that_integer . length . returns $ c
        array_decl = "return_stack_type[50] return_stack;"
        array_type = "header return_stack_type { int<"++ "16" ++"> val;}" --"header return_stack_type { bit<"++ show s ++"> val;}"
    in 
        indentation (indent c) ++ array_type ++ "\n"
        ++ indentation (indent c) ++ array_decl ++ "\n"
        ++ indentation (indent c) ++ "int return_stack_index = 0;"


-- bmv2 requires that all header fields are multiples of 8 bits
integer_to_n_bits_needed_to_hold_that_integer :: Int -> Int
integer_to_n_bits_needed_to_hold_that_integer i = 1 + floor (logBase 2 (fromIntegral i :: Double))


{- names:-}
return_stack_index :: String
return_stack_index = "return_stack_index"