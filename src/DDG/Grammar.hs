{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Grammar where
import Data.List ( intercalate)
import qualified Data.Map as M
import Control.Monad.State.Lazy
{- 
Corresponds to Definition 1 of:
    "Semantics and Algorithms for Data-dependent Grammars", Jim, Mandelbaum & Walker 
-}

data Rule = 
    KleineClosure Rule
    | Alternation Rule Rule
    | Sequence Rule Rule
    | RuleLabel Label 
    deriving (Show, Eq)


data Label = 
    Epsilon
    | Empty
    | Terminal String
    | NonTerminalCall NonTerminalId Expression
    | Statements [String]
    | Constraint Expression 
    deriving (Show, Eq, Ord)

data Nonterminal = Nonterminal NonTerminalId [Param] Rule deriving (Show)

data Param = Param Type Id deriving (Show)
-- (Σ, ∆, Φ, A0, R)
-- Since we are embedding data-dependent grammars in P4 we omit terminals from our grammar definition
-- We also don't need the have mapping from nonterminals to rules, we include this in the definition of Nonterminal above
-- and we assume the first nonterminal is the start nonterminal 
type Grammar =  [Nonterminal]


type NonTerminalId = String
type Id = String
type Expression = String
type Type = String
pp_l :: Label -> String
pp_l Epsilon = "e"
pp_l Empty = "empty"
pp_l (Terminal s) = s
pp_l (NonTerminalCall n e) = n ++ "(" ++ e ++ ")"
-- pp_l (Bindings b) = intercalate "\n" $ map pp_binding b
pp_l (Statements s) = intercalate "\n" s

pp_l (Constraint e) = "[" ++ e ++ "]"

pp_binding (t, n, e) = 
    t ++ " "++ n ++ ":=" ++ e

pp :: Rule -> String
pp (KleineClosure r) = "(" ++ pp r ++ ")" ++ "*"
pp (Alternation r1 r2) = "(" ++ "(" ++ pp r1 ++ ")" ++ "|" ++ "(" ++ pp r2 ++ ")"++ ")"
pp (Sequence r1 r2) = "(" ++ pp r1 ++ ")" ++ "(" ++ pp r2 ++ ")"
pp (RuleLabel l) = pp_l l

pp_Grammar :: Grammar -> String
pp_Grammar = unlines . map pp_nonterminal
    where
        pp_nonterminal (Nonterminal n [] r) = n ++ " = " ++ pp r
        pp_nonterminal (Nonterminal n params r) = n ++ "(" ++ pp_params  params++ ") = " ++ pp r
pp_params :: [Param] -> String
pp_params params = intercalate ", " $ map print params 
    where 
        print (Param t id) = t ++ " " ++ id 

-- inlineNonterminaslWithoutParams :: Grammar -> Grammar
-- inlineNonterminalsWithoutParams g= 
--     let noParams = [x | x@(Nonterminal _ [] _) <- g] in
--     foldl 

-- inline :: Nonterminal -> Grammar -> Grammar 

{- 
specialize :: Grammar -> Grammar

Create a variant for every nonterminal
 -}

type CountMap = M.Map String Int
type CountM = State CountMap

visitLabel :: Label -> CountM ()
visitLabel (NonTerminalCall n _) = modify (M.insertWith (+) n 1)
visitLabel _ = return ()

visitRule :: Rule -> CountM ()
visitRule (KleineClosure r) = visitRule r
visitRule (Alternation r1 r2) = visitRule r1 >> visitRule r2
visitRule (Sequence r1 r2) = visitRule r1 >> visitRule r2
visitRule (RuleLabel l) = visitLabel l

countNonterminalCalls :: Grammar -> CountMap
countNonterminalCalls g = execState (mapM_ visitNonterminal g) M.empty
  where
    visitNonterminal (Nonterminal _ _ r) = visitRule r


-- visit_label :: Grammar -> (Label -> )

-- specialize :: Grammar -> Grammar


-- create_specialized_calls :: String -> (Integer, Label) -> (Integer, Label)
-- create_specialized_calls nonterminal (count, (NonTerminalCall n e)) = 
--     (count + 1, (NonTerminalCall (n ++ "_ " ++ show count) e))
-- create_specialized_calls nonterminal (count, l) = (count, l)

-- create_specialized_nonterminals :: Integer -> Nonterminal -> [Nonterminal]
-- create_specialized_nonterminals count (Nonterminal name params body) = [Nonterminal (name ++ "_" ++ show c) | c <- [0..c]]