{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}
module CodeGen.Continuation where
import GrammarToTransducer
import Data.Graph.Inductive
import qualified GrammarToTransducer as T

import GrammarToTransducer
import Grammar
import qualified Data.Map as M
import Data.List (intercalate)
-- data v = P4Transducer

-- data Action = 
--     Extract String
--     | Statements [String]

-- data StateLabel = StateLabel StateType [Action]

-- data StateType = Output | Normal
type State = Int
-- data Transition = 
--     GuardedTransition Expression State
--     | UnGuardedTransition State
--     | ReturnTransition
--     | CallTransition ContinuationId

data P4Transition =
    If E [Stmt] State -- Goto S at the end
    | Goto State
    | Accept
    deriving (Show, Eq)

data Stmt =
    Extract String
    | Do [String] -- Host language stmts
    | Params [String] [E]
    | Push Int deriving (Show, Eq)

type TransducerFormatted = [(State, [(T.Transition, State)])]
type P4Transducer = [(State, [Stmt], [P4Transition])]

type E = String

type Continuations = M.Map Int Int

-- format :: T.Transducer -> Transducer

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
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
get_continuation :: Int -> M.Map Int Int -> (Int, M.Map Int Int)

get_continuation state c = 
    case M.lookup state c of
            Just e -> (e, c)
            Nothing -> (1+(M.size c), M.insert state (1+(M.size c)) c) 

convert :: Grammar -> (TransducerFormatted, P4Transducer, Continuations) -> (TransducerFormatted, P4Transducer, Continuations)
convert g ([(s1, [] )], xs, cs)  =
    -- assert s1 == s1'
    ([], xs, cs )
convert g(((s1, [] ):(s2, out):transducer), xs, cs)  =
    -- assert s1 == s1'
    (((s2, out):transducer), ((s2, [], []):xs), cs )

convert g (([]), xs, cs)  =
    -- assert s1 == s1'
    ([], xs, cs)
convert g (x@(s1, _):transducer, [], cs)  =
    (x:transducer, [(s1, []::[Stmt], []::[P4Transition])], cs)

convert g (transducer, p4transducer, c) = 
    let 
        (s1, (h : out)):rest_transducer = transducer
        (s1', stmts, trans):xs = p4transducer
        remve_one_trans = (s1, out):rest_transducer
         in
    case h of 
        (Labeled (Constraint e), s2) -> 
            if any (not . is_constraint ) out then 
                let else_cases = filter (not . is_constraint) out in
                if length else_cases /= 1 then
                    error "For a state with guarded transitions, there are more than 1 [else] transitions"
                else error $ "not implemented : " ++ (show (s1, (h : out)))

            else (
                                            remve_one_trans
                                            , ((s1', stmts, trans ++ [If e [] s2]):xs)
                                            , c
                                        )
        (Labeled (Terminal t), s2) -> (
                                            remve_one_trans
                                            , ((s1', stmts ++ [Extract t], trans ++ [Goto s2]):xs)
                                            , c
                                        )   

        (Labeled (Statements s), s2) -> (
                                            remve_one_trans
                                            , ((s1', stmts ++ [Do s], trans ++ [Goto s2]):xs)
                                            , c
                                        )
        (Call f1 x1, s2)             ->
            let 
                filt = (\case (Labeled(NonTerminalCall f2 x2), _) -> (f1 == f2 && x1 == x2); _ -> False)
                -- remove corresponding return transition
                [(Labeled(NonTerminalCall f2 x2), s3)] = filter filt out
                new_out = filter (\x -> not . filt $ x) out
                new_transducer = (s1, new_out):rest_transducer
                (continuation, new_c) = get_continuation s3 c
                ps = map (\(Param _ id) -> id) $ params g f1
                es = wordsWhen (== ',') x1
            in
             (
                                            new_transducer
                                            ,((s1', stmts ++ [Params ps es, Push continuation], trans ++ [Goto s2]):xs)
                                            , new_c
                                        )


        
        (Labeled(NonTerminalCall f2 x2),s3) ->
             let 
                filt = (\case (Call f1 x1, _) -> (f1 == f2 && x1 == x2); _ -> False)
                [(Call f1 x1, s2)] = filter filt out
                new_out = filter (\x -> not . filt $ x) out
                new_transducer = (s1, new_out):rest_transducer
                (continuation, new_c) = get_continuation s3 c
                ps = map (\(Param _ id) -> id) $ params g f1
                es = wordsWhen (== ',') x1
            in

                            (
                            new_transducer
                            ,((s1', stmts ++ [Params ps es, Push continuation], trans ++ [Goto s2]):xs)
                            , new_c
                        )


        (Output _, s2) -> 
            (
                remve_one_trans
                , ((s1', stmts, trans++ [Accept]):xs)
                , c
            )
convert g (a, b, c) = 
    error $ "convert: " ++ show a ++ show b ++ show c

is_constraint ((Labeled (Constraint _)), _) = True
is_constraint _ = False
{- convert g (((s1, (Output _, s2) : out):transducer), ((s1', stmts, trans):xs), cs)  =
    -- assert s1 == s1'
    (((s1, out):transducer), ((s1', stmts, trans++ [Accept]):xs), cs)
 -}
        

-- constraint
{- convert g (((s1, (Labeled (Constraint e), s2):out):transducer), ((s1', stmts, trans):xs), cs) =
    -- assert s1 == s1'
    (((s1, out):transducer), ((s1', stmts, trans ++ [If e [] s2]):xs) , cs)
 -}
-- -- terminal{- 
-- convert g (((s1, (Labeled (Terminal t), s2):out):transducer), ((s1', stmts, trans):xs), cs)  =
--     -- assert s1 == s1'
--     (((s1, out):transducer), ((s1', stmts ++ [Extract t], trans ++ [Goto s2]):xs), cs) -}
-- convert g (((s1, (Labeled (Statements s), s2):out):transducer), ((s1', stmts, trans):xs), cs)  =
--     -- assert s1 == s1'
--     (((s1, out):transducer), ((s1', stmts ++ [Do s], trans ++ [Goto s2]):xs), cs)

-- call-return
{- convert g (((s1, (Call f1 x1, s2):(Labeled(NonTerminalCall f2 x2),s3):out):transducer), ((s1', stmts, trans):xs), cs)  =
    let (continuation, new_cs) = case M.lookup s3 cs of
            Just c -> (c, cs)
            Nothing -> (1+(M.size cs), M.insert s3 (1+(M.size cs)) cs) 
        ps = map (\(Param _ id) -> id) $ params g f1
        es = wordsWhen (== ',') x1
        in
    -- assert s1 == s1'
    -- assert f1 == f2
    -- assert x1 == x2
    (((s1, out):transducer), ((s1', stmts ++ [Params ps es, Push continuation], trans ++ [Goto s2]):xs), new_cs )
 -}
-- convert g (((s1, (Output _, s2) : out):transducer), ((s1', stmts, trans):xs), cs)  =
--     -- assert s1 == s1'
--     (((s1, out):transducer), ((s1', stmts, trans++ [Accept]):xs), cs)


-- convert g ([(s1, [] )], xs, cs)  =
--     -- assert s1 == s1'
--     ([], xs, cs )
-- convert g(((s1, [] ):(s2, out):transducer), xs, cs)  =
--     -- assert s1 == s1'
--     (((s2, out):transducer), ((s2, [], []):xs), cs )

-- convert g (([]), xs, cs)  =
--     -- assert s1 == s1'
--     ([], xs, cs)
-- convert g (x@(s1, _):transducer, [], cs)  =
--     (x:transducer, [(s1, []::[Stmt], []::[P4Transition])], cs)
convert':: Grammar -> (TransducerFormatted, P4Transducer, Continuations) -> (P4Transducer, Continuations)
convert' g i =
    let (n1, n2, n3) = convert g i
    in
        if n1 == [] then
            (n2, n3)
        else
            convert' g (n1, n2, n3)

-- convert (((s1, (Call f x, s2):out):transducer), ((s1', stmts, trans):xs), cs)  = 
--     -- assert s1 == s1'
--     (((s, out):transducer), ((s1', stmts ++ [Params f x], trans ++ [Goto s2]):xs) )

optimize :: P4Transducer -> P4Transducer -> P4Transducer
optimize [] xs = xs
optimize (x@(s1, stmts, [Goto s2]):xs) constructed =
    let
        new_todo = reverse $ inline x xs []
        new_constructed = reverse $ inline x constructed []
    in
        if xs == new_todo && constructed == new_constructed then
            -- error $ show x ++ show xs ++ show new_todo
            optimize xs (x:constructed)
        else
            -- error $ show x ++ show xs ++ show new_todo ++ show (xs == new_todo )
            optimize new_todo new_constructed
-- optimize (x@(s1, stmts, [If s2]):xs) constructed = optimize
--     (inline x xs) (inline x constructed)
optimize (x:xs) constructed = optimize xs (x:constructed)

transducer_to_p4 :: Grammar -> TransducerFormatted -> (P4Transducer, Continuations)
transducer_to_p4 g t = convert' g (t, [], M.empty)


inline :: (State, [Stmt], [P4Transition]) -> P4Transducer -> P4Transducer -> P4Transducer
inline _ [] constructed = constructed
inline (x@(state, stmts, [Goto target])) ((s, stmts2, trans2):xs) constructed =
    let ifs = [if_ | if_@(If _ _ to) <- trans2, to == state] in
    
    if (Goto state) `elem` trans2 then
        let new_trans2 = map (\t -> if t == Goto state then Goto target else t) trans2
        in
            inline x xs ((s, stmts2 ++ stmts, new_trans2):constructed)
        -- update_goto 
    -- else -- check guarded trans 
    -- if not . null $ ifs then
    --     let 
    --         new_trans2 = update_ifs (state, stmts) trans2 in
    --     inline x xs ((s, stmts2 ++ stmts, new_trans2):constructed)
    else
        inline x xs ((s, stmts2, trans2):constructed)

update_ifs :: (State, [Stmt]) -> [P4Transition] -> [P4Transition]
update_ifs to_inline trans= map (update_if to_inline) trans

update_if :: (State, [Stmt]) -> P4Transition -> P4Transition
update_if(target, stmts) (If e stmts2 target2) =
    if target == target2 then
        If e (stmts ++ stmts2) target
    else
        If e stmts2 target2
update_if _ x = x


format :: T.Transducer -> TransducerFormatted
format transducer =
    let
        nodes = labNodes (T.graph transducer)
    in
        map (findEdges (T.graph transducer) ) nodes
    where
        findEdges :: T.TransducerGraph -> (LNode ()) -> (State, [(T.Transition, State)])
        findEdges graph node =
            let o = lsuc graph (to_state node) in
            (to_state node , map (\(node_id,label) -> (label, node_id)) o )

        to_state :: LNode () -> State
        to_state (node_id, _) = node_id

to_p4 :: Grammar -> Continuations -> P4Transducer -> String
to_p4 g c t =
    (gen_preamble g t (length c + 1 ) 16) ++ gen_states t ++ gen_return_state c

gen_preamble :: Grammar -> P4Transducer -> Int -> Int -> String
gen_preamble grammar p4t n_continuations stack_size= 
        "// Place this header definition outside of your parser\n" ++
        "header return_stack_type { bit<"++ show (compute_bit_width_8 n_continuations) ++ "> val;}\n" ++
        "// Make these global variables of the parser\n" ++
        "return_stack_type["++ show stack_size++"] return_stack;\n" ++
        "bit<"++ show( compute_bit_width_8 stack_size) ++ "> return_stack_index = 0;\n" ++
        gen_fun_param_decls grammar

gen_return_state :: Continuations -> String
gen_return_state c = let 
    size = show (compute_bit_width_8 (length c+1)) in 
    "state state_continue {\n" ++
    "    bit<"++ size ++"> tmp_return;\n" ++
    "    if (return_stack_index == 0) { tmp_return = 0;} else {\n" ++
    "        tmp_return = return_stack[return_stack_index].val;\n" ++
    "        return_stack_index = return_stack_index - 1;\n" ++
    "    }\n" ++
    "    transition select (tmp_return) {\n" ++
    concatMap (\(k, v) -> "           " ++ show v ++ " : state_" ++ show k ++ ";\n") (M.toList c) ++
    "           0 : accept;\n" ++
    "    }\n" ++
    "}\n"

gen_fun_param_decls :: Grammar -> String
gen_fun_param_decls g = 
    let all_params = concat $ [p | (Nonterminal _ p _) <-g ] in
         concatMap (\(Param t i) -> t ++ " " ++ i ++ ";\n") all_params

-- TODO, needs to be 8 bits increments 
compute_bit_width_8 :: Int -> Int
compute_bit_width_8 x = ((ceiling (logBase 2 (fromIntegral x)) + 7) `div` 8) * 8

gen_states :: P4Transducer -> String
gen_states t = intercalate "\n" $ map gen_state t

gen_state :: (State, [Stmt], [P4Transition]) -> String
gen_state (name, stmts, trans) =
    let
        ifs = [x | x@(If _ _ _) <- trans ]
        other_transitions = [ t | t <- trans, t `notElem` ifs] -- This is terrible
        begin = "state state_" ++ show name ++ " {\n" ++ concatMap gen_stmt stmts
    in
        if null ifs then
                begin ++ concatMap gen_tran other_transitions ++ "    }\n"
        else
            if not $ null other_transitions then 
                if length other_transitions == 1 then
                    case head other_transitions of
                        (Goto s) -> begin ++ gen_ifs (Just ("state_" ++ show s)) (zipWith (curry (\(If a b c, i)-> ((a, b, c), i))) ifs [1..] ) ++ "    }\n"
                        (Accept) -> begin ++ gen_ifs (Just "state_continue") (zipWith (curry (\(If a b c, i)-> ((a, b, c), i))) ifs [1..] ) ++ "    }\n"


                    
                else
                    error "gen_state : both guarded and unguarded transitions  id:" ++ show ( length other_transitions)
                    
                -- show stmts ++ "\n" ++ show trans ++ "\n " ++ show other_transitions
            else
                begin ++ gen_ifs (Nothing) (zipWith (curry (\(If a b c, i)-> ((a, b, c), i))) ifs [1..] ) ++ "    }\n"


gen_stmt :: Stmt -> String
gen_stmt (Extract s) = "    packet.extract(" ++ s ++ ");\n"
gen_stmt (Push c) =
    "    return_stack_index = return_stack_index + 1;\n"++
    "    return_stack[return_stack_index].val = "++show c++";\n"
gen_stmt (Params vars exprs) =
    concatMap (\(v,e) -> "    " ++ v ++ " = " ++ e ++";\n") $ zip vars exprs

gen_stmt (Do s) = concatMap (\x -> "    " ++ x ++ "\n") s

gen_tran :: P4Transition -> String
gen_tran (Goto s) = "    transition state_" ++ show s ++ ";\n"
gen_tran (Accept) = "    transition state_continue;\n"

gen_ifs :: Maybe String -> [((E, [Stmt], State), Int)] -> String
gen_ifs def ifs =
    let bit_length =compute_bit_width_8 $ length ifs + 1 in
    "    " ++ "bit<"++ show bit_length ++"> tmp = 0;\n" ++
    concatMap gen_if ifs ++
    "    transition select(tmp) {\n" ++
    (concatMap (\((_, _, s), c) -> "       " ++ show c ++ " : " ++ "state_" ++ show s ++ ";\n") ifs) ++
    ((\case Nothing -> "" ; Just s -> "       0 :" ++ s ++ ";\n") def) ++
     "}\n"

gen_if :: ((E, [Stmt], State), Int) -> String
gen_if ((e, stmts, _), c) =
    "    if ("++ e ++")" ++ "{\n" ++
            concatMap (("    " ++ ) . gen_stmt) stmts ++
    "        " ++ "tmp = " ++ show c ++ ";\n" ++

    "    }"