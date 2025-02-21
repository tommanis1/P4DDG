{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Grammar where
{- 
Corresponds to Definition 1 of:
    "Semantics and Algorithms for Data-dependent Grammars", Jim, Mandelbaum & Walker 
-}

data Rule = 
    KleineClosure Rule
    | Alternation Rule Rule
    | Sequence Rule Rule
    | RuleLabel Label deriving (Show, Eq)


data Label = 
    Epsilon
    | Empty
    | Terminal String
    | NonTerminalCall NonTerminalId Expression
    | Binding Id Expression
    | Constraint Expression deriving (Show, Eq, Ord)

data Nonterminal = Nonterminal NonTerminalId [String] Rule

-- (Σ, ∆, Φ, A0, R)
-- Since we are embedding data-dependent grammars in P4 we omit terminals from our grammar definition
-- We also don't need the have mapping from nonterminals to rules, we include this in the definition of Nonterminal above
-- and we assume the first nonterminal is the start nonterminal 
type Grammar =  [Nonterminal]


type NonTerminalId = String
type Id = String
type Expression = String

pp_l :: Label -> String
pp_l Epsilon = "e"
pp_l Empty = "empty"
pp_l (Terminal s) = s
pp_l (NonTerminalCall n e) = n ++ "(" ++ e ++ ")"
pp_l (Binding i e) = i ++ ":=" ++ e
pp_l (Constraint e) = "[" ++ e ++ "]"

pp :: Rule -> String
pp (KleineClosure r) = "(" ++ pp r ++ ")" ++ "*"
pp (Alternation r1 r2) = "(" ++ "(" ++ pp r1 ++ ")" ++ "|" ++ "(" ++ pp r2 ++ ")"++ ")"
pp (Sequence r1 r2) = "(" ++ pp r1 ++ ")" ++ "(" ++ pp r2 ++ ")"
pp (RuleLabel l) = pp_l l

pp_Grammar :: Grammar -> String
pp_Grammar = unlines . map pp_nonterminal
    where
        pp_nonterminal (Nonterminal n [] r) = n ++ " = " ++ pp r
        pp_nonterminal (Nonterminal n args r) = n ++ "(" ++ unwords args ++ ") = " ++ pp r