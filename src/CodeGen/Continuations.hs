{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CodeGen.Continuations where

import qualified Transducer.Def as T
import P4Types
import Data.Graph.Inductive hiding(Node, Edge, Graph)
import qualified DDG.P4DDG hiding (Stmt)
import DDG.Types (Grammar, Label(..), Rule(..), Param(..), Nonterminal(..), NonTerminalId)
import Data.List (intercalate, find, partition)
import qualified Data.Map as M
import qualified Data.Set as Set

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

-- | LLM generated; Build a map from nonterminal names to unique identifiers for those with multiple callees
buildContinuationMap :: Grammar -> M.Map String Integer
buildContinuationMap g = 
    let nonterminals = [n | (Nonterminal n _ _) <- g]
        callees = findAllCallees g
        calleeCounts = [(n, length $ filter (== n) callees) | n <- nonterminals]
        multipleCallees = [n | (n, count) <- calleeCounts, count > 1]
    in M.fromList $ zip multipleCallees [1..]

-- | Find all callees in the grammar
findAllCallees :: Grammar -> [NonTerminalId]
findAllCallees grammar = concatMap calleesInRule [rule | Nonterminal _ _ rule <- grammar]
  where
    calleesInRule :: Rule (DDG.P4DDG.E P4Types.Expression) -> [NonTerminalId]
    calleesInRule (KleineClosure r) = calleesInRule r
    calleesInRule (Alternation r1 r2) = calleesInRule r1 ++ calleesInRule r2
    calleesInRule (Sequence r1 r2) = calleesInRule r1 ++ calleesInRule r2
    calleesInRule (Label l) = calleesInLabel l

    calleesInLabel :: Label e -> [NonTerminalId]
    calleesInLabel (NonTerminalCall ntId _) = [ntId]
    calleesInLabel _ = []

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
                    ps :: [String] = map (\(Param _ id) -> id) $ params grammar fname1
                    es :: [String] = wordsWhen (== ',') e1
                    assign_params =  map (\(a, b) -> Bind a b) (zip ps es)
                    push_continuation = case M.lookup fname1 (buildContinuationMap grammar) of
                        Just c ->  [Push c]
                        Nothing -> []-- When a nonterminal is not in the map, we assume that it has only one callee and thus there is no need for a continuation
                    nodes = [(stateId, assign_params ++ push_continuation)]
                    edges = [(stateId, s2, Otherwise)]
                in
                    mkGraph' nodes edges
            
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

mkP4 :: Grammar -> P4Transducer -> String
mkP4 grammar t =
    let
        start = T.start t
        ids = map fst $ labNodes (T.graph t)
        stack_size = 16
        n_continuations = 12
        contains_param_calls = False
        name = "TMP"
        parserDecl = 
            "parser " ++ name ++ " (packet_in packet,\n"
            ++ "    " ++ "out headers hdr,\n"
            ++ "    " ++ "inout metadata meta,\n"
            ++ "    " ++ "inout standard_metadata_t standard_metadata) {\n"
        handle_continuations = if M.null (buildContinuationMap grammar) then "" else genReturnState (buildContinuationMap grammar)
    in
        parserDecl ++ "\n" ++
        "// Place this header definition outside of your parser\n" ++
        if contains_param_calls then
        "header return_stack_type { bit<"++ show (compute_bit_width_8 n_continuations) ++ "> val;}\n"
        else ""
        ++ 
        "// Make these global variables of the parser\n" ++
        if contains_param_calls then
        "return_stack_type["++ show stack_size++"] return_stack;\n" ++
        "bit<"++ show( compute_bit_width_8 stack_size) ++ "> return_stack_index = 0;\n"
        else

       "state start {\n" ++
        "    transition state_" ++ show start ++ ";\n" ++
        "}\n" ++
        concatMap (\i -> mkState i t) ids
        ++ "}\n"
        ++ handle_continuations



mkState :: Int -> P4Transducer -> String
mkState s1 t =
    let
        (nodes, edges) = (labNodes (T.graph t), labEdges (T.graph t))
        outgoing =  filter (\(s, _, _) -> s == s1) edges
        body = mkBody $ snd $ head $ filter (\(n, _) -> n == s1) nodes
    in
        "state state_" ++ show s1 ++ " {\n" ++
            appendToLines "    " body ++
            appendToLines "    " (mkOutgoingTransitions outgoing) ++
        "}\n"

mkOutgoingTransitions :: [(Int, Int, Transition (DDG.P4DDG.E P4Types.Expression))]  -> String
mkOutgoingTransitions [(s1, s2, Otherwise)]  =
    "transition state_" ++ show s2 ++ ";\n"
mkOutgoingTransitions out  =
    let
        (ifs, otherwises) = partition (\case (_, _, Otherwise) -> False; _ -> True) out
        otherwise :: Maybe Int = findOtherwise otherwises
    in  
        if all_same_type ifs then
            error "mkState: ifs have different types: not yet implemented"
            -- let left_side_of_expression = "" in
            --     "select (" ++ left_side_of_expression ++ ") {\n" ++
            --         concatMap (\((_, s2, _), right_side) ->
            --             "    " ++ right_side ++ " : state_" ++ show s2 ++ ";\n") ifs
            --         ++ case otherwise of
            --             Just s2 -> "    default : state_" ++ show s2 ++ ";\n"
            --             Nothing -> ""
            --     ++ "}\n"

        else
            let bit_length = compute_bit_width_8 $ length ifs + 1 in
                "bit<"++ show bit_length ++"> tmp = 0;\n"
                ++ concatMap (gen_if . (\(_, s2, E e) -> (e,s2))) ifs
                ++ "transition select(tmp) {\n" ++
                    concatMap (\((_, s2, _), i) ->
                        "    " ++ show i ++ " : state_" ++ show s2 ++ ";\n") (zip ifs [1..])
                    ++ case otherwise of
                        Just s2 -> "    0 : state_" ++ show s2 ++ ";\n"
                        Nothing -> ""
                ++ "}\n"

            -- error $ "mkState: ifs have different types: not yet implemented"



    where
        findOtherwise [] = Nothing
        findOtherwise [(s1, s2, Otherwise)] = Just s2
        findOtherwise x = error $ "findOtherwise: more than one otherwise where one was expected" ++ show x

        gen_if :: (DDG.P4DDG.E P4Types.Expression, Int) -> String
        gen_if (e, c) =
            "if ("++ expressionToP4 e ++")" ++ "{\n" ++
            "    " ++ "tmp = " ++ show c ++ ";\n" ++
            "}\n"

        all_same_type x = False -- TODO: implement this
mkBody :: [Stmt P4Types.Statement] -> String
mkBody [] = ""
mkBody smts = concatMap (codegenStmt) smts

codegenStmt :: Stmt P4Types.Statement -> String
codegenStmt (Extract s) = "packet.extract(" ++ s ++ ");\n"
codegenStmt (Bind s1 s2) = s1 ++ " = " ++ s2 ++ ";\n"
codegenStmt (HostLanguageStmts stmts) = intercalate "\n" stmts
codegenStmt (Push i) =
    "return_stack_index = return_stack_index + 1;\n"
    ++ "return_stack[return_stack_index].val = "++show i++";\n"


compute_bit_width_8 :: Int -> Int
compute_bit_width_8 x = ((ceiling (logBase 2 (fromIntegral x)) + 7) `div` 8) * 8

expressionToP4 :: (DDG.P4DDG.E P4Types.Expression) -> String
expressionToP4 (DDG.P4DDG.E e) = ppP4E e
expressionToP4 (DDG.P4DDG.Not e) = "!" ++ expressionToP4 e

appendToLines :: String -> String -> String
appendToLines prefix str =
    let
        lines' = lines str
        prefixedLines = map (prefix ++) lines'
    in
        unlines prefixedLines

genReturnState :: M.Map String Integer -> String
genReturnState c = let 
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