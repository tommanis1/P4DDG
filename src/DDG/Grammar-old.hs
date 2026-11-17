{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ExistentialQuantification #-}
module DDG.Grammar where
import Data.List ( intercalate)
import qualified Data.Map as M
import Control.Monad.State.Lazy
{- 
Corresponds to Definition 1 of:
    "Semantics and Algorithms for Data-dependent Grammars", Jim, Mandelbaum & Walker 
-}

data Show l => Rule l = 
    KleineClosure (Rule l)
    | Alternation (Rule l) (Rule l)
    | Sequence (Rule l) (Rule l)
    | Label l
    deriving (Show, Eq)


data Label =
    Epsilon
    -- | Empty
    | Terminal String
    | NonTerminalCall NonTerminalId Expression
    | Statements [String]
    | forall e. (Show e, Eq e) => Constraint e

instance Show Label where
    show Epsilon = "epsilon"
    -- show Empty = "empty"
    show (Terminal s) = s
    show (NonTerminalCall n e) = n ++ "(" ++ e ++ ")"
    show (Statements s) = intercalate "\n" s
    show (Constraint e) = "[" ++ show e ++ "]"

instance Eq Label where
    Epsilon == Epsilon = True
    -- Empty == Empty = True
    Terminal s1 == Terminal s2 = s1 == s2
    NonTerminalCall n1 e1 == NonTerminalCall n2 e2 = n1 == n2 && e1 == e2
    Statements s1 == Statements s2 = s1 == s2
    Constraint e1 == Constraint e2 = show e1 == show e2 -- This is a pretty nifty hack

instance Ord Label where
    compare Epsilon Epsilon = EQ
    compare Epsilon _ = LT
    compare _ Epsilon = GT
    
    -- compare Empty Empty = EQ
    -- compare Empty Epsilon = GT
    -- compare Empty _ = LT
    -- compare _ Empty = GT
    
    compare (Terminal s1) (Terminal s2) = compare s1 s2
    compare (Terminal _) Epsilon = GT
    -- compare (Terminal _) Empty = GT
    compare (Terminal _) _ = LT
    compare _ (Terminal _) = GT
    
    compare (NonTerminalCall n1 e1) (NonTerminalCall n2 e2) = 
        case compare n1 n2 of
            EQ -> compare e1 e2
            result -> result
    compare (NonTerminalCall _ _) Epsilon = GT
    -- compare (NonTerminalCall _ _) Empty = GT
    compare (NonTerminalCall _ _) (Terminal _) = GT
    compare (NonTerminalCall _ _) _ = LT
    compare _ (NonTerminalCall _ _) = GT
    
    compare (Statements s1) (Statements s2) = compare s1 s2
    compare (Statements _) Epsilon = GT
    -- compare (Statements _) Empty = GT
    compare (Statements _) (Terminal _) = GT
    compare (Statements _) (NonTerminalCall _ _) = GT
    compare (Statements _) _ = LT
    compare _ (Statements _) = GT
    
    compare (Constraint e1) (Constraint e2) = compare (show e1) (show e2)
    compare (Constraint _) _ = GT
    compare _ (Constraint _) = LT
    
data Nonterminal = Nonterminal NonTerminalId [Param] (Rule (Label)) deriving (Show)

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
-- pp_l Empty = "empty"
pp_l (Terminal s) = s
pp_l (NonTerminalCall n e) = n ++ "(" ++ e ++ ")"
-- pp_l (Bindings b) = intercalate "\n" $ map pp_binding b
pp_l (Statements s) = intercalate "\n" s

pp_l (Constraint e) = "[" ++ (show e) ++ "]"

pp_binding (t, n, e) = 
    t ++ " "++ n ++ ":=" ++ e

pp :: Rule Label -> String
pp (KleineClosure r) = "(" ++ pp r ++ ")" ++ "*"
pp (Alternation r1 r2) = "(" ++ "(" ++ pp r1 ++ ")" ++ "|" ++ "(" ++ pp r2 ++ ")"++ ")"
pp (Sequence r1 r2) = "(" ++ pp r1 ++ ")" ++ "(" ++ pp r2 ++ ")"
pp (Label l) = pp_l l

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
 
-- specialize :: Grammar -> Grammar
-- -- Create a variant for every nonterminal with more than one callee
-- specialize g = 
--     let m = M.filter (>1) (countNonterminalCalls g) in
--         update_Nonterminal m create_specialized_nonterminals g
    
    

{- type CountMap = M.Map String Int
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


-- update_Nonterminal :: a -> ((a, Nonterminal) -> (a,Nonterminal)) -> Grammar -> Grammar
-- update_Nonterminal start_context f grammar = 


update_Nonterminal :: a -> ((a, Nonterminal) -> (a, Nonterminal)) -> Grammar -> Grammar
update_Nonterminal startContext transformFn grammar = 
    -- Apply the transformation function to each nonterminal in the grammar
    -- while threading through the context value
    let (_, updatedGrammar) = foldr (processNonterminal transformFn) (startContext, []) grammar
    in updatedGrammar
  where
    processNonterminal :: ((a, Nonterminal) -> (a, Nonterminal)) 
                       -> Nonterminal 
                       -> (a, Grammar) 
                       -> (a, Grammar)
    processNonterminal f nonterminal (context, accGrammar) =
        let (updatedContext, updatedNonterminal) = f (context, nonterminal)
        in (updatedContext, updatedNonterminal : accGrammar)
-- visit_label :: Grammar -> (Label -> )

-- specialize :: Grammar -> Grammar

create_specialized_nonterminals :: (CountMap, Nonterminal) -> (CountMap, Nonterminal)
create_specialized_nonterminals (cm, f@(Nonterminal name params body)) = 
    case M.lookup name cm of 
        Nothing -> (cm, f)
        (Just count) -> 
            (M.adjust (\x -> x - 1) name cm, (Nonterminal (name ++ "_" ++ show count) params body ))

create_specialized_calls :: String -> (Integer, Label) -> (Integer, Label)
create_specialized_calls nonterminal (count, (NonTerminalCall n e)) = 
    (count + 1, (NonTerminalCall (n ++ "_ " ++ show count) e))
create_specialized_calls nonterminal (count, l) = (count, l)

-- create_specialized_nonterminals :: Integer -> Nonterminal -> [Nonterminal]
-- create_specialized_nonterminals count (Nonterminal name params body) = [Nonterminal (name ++ "_" ++ show c) | c <- [0..c]] p -}