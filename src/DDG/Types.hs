{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module DDG.Types where
import DDG.P4DDG 
import P4Types hiding (pp)
import Data.List (intercalate)

data Show e => Rule e = 
    KleineClosure (Rule e)
    | Alternation (Rule e) (Rule e)
    | Sequence (Rule e) (Rule e)
    | Label (Label e)
    deriving (Show, Eq, Ord)


data Label e =
    Epsilon
    | Empty
    | Terminal String
    | NonTerminalCall NonTerminalId String
    | Statements [String]
    | Constraint e
    deriving (Show, Eq, Ord)

    -- | forall e. (Show e, Eq e) => Constraint e

-- instance Show Label where
--     show Epsilon = "epsilon"
--     show Empty = "empty"
--     show (Terminal s) = s
--     show (NonTerminalCall n e) = n ++ "(" ++ e ++ ")"
--     show (Statements s) = intercalate "\n" s
--     show (Constraint e) = "[" ++ show e ++ "]"

-- instance Eq Label where
--     Epsilon == Epsilon = True
--     Empty == Empty = True
--     Terminal s1 == Terminal s2 = s1 == s2
--     NonTerminalCall n1 e1 == NonTerminalCall n2 e2 = n1 == n2 && e1 == e2
--     Statements s1 == Statements s2 = s1 == s2
--     Constraint e1 == Constraint e2 = show e1 == show e2 -- This is a pretty nifty hack

-- instance Ord Label where
--     compare Epsilon Epsilon = EQ
--     compare Epsilon _ = LT
--     compare _ Epsilon = GT
    
--     compare Empty Empty = EQ
--     compare Empty Epsilon = GT
--     compare Empty _ = LT
--     compare _ Empty = GT
    
--     compare (Terminal s1) (Terminal s2) = compare s1 s2
--     compare (Terminal _) Epsilon = GT
--     compare (Terminal _) Empty = GT
--     compare (Terminal _) _ = LT
--     compare _ (Terminal _) = GT
    
--     compare (NonTerminalCall n1 e1) (NonTerminalCall n2 e2) = 
--         case compare n1 n2 of
--             EQ -> compare e1 e2
--             result -> result
--     compare (NonTerminalCall _ _) Epsilon = GT
--     compare (NonTerminalCall _ _) Empty = GT
--     compare (NonTerminalCall _ _) (Terminal _) = GT
--     compare (NonTerminalCall _ _) _ = LT
--     compare _ (NonTerminalCall _ _) = GT
    
--     compare (Statements s1) (Statements s2) = compare s1 s2
--     compare (Statements _) Epsilon = GT
--     compare (Statements _) Empty = GT
--     compare (Statements _) (Terminal _) = GT
--     compare (Statements _) (NonTerminalCall _ _) = GT
--     compare (Statements _) _ = LT
--     compare _ (Statements _) = GT
    
--     compare (Constraint e1) (Constraint e2) = compare (show e1) (show e2)
--     compare (Constraint _) _ = GT
--     compare _ (Constraint _) = LT
    
data Nonterminal = Nonterminal NonTerminalId [Param] (Rule (DDG.P4DDG.E P4Types.Expression)) deriving (Show)

data Param = Param Type Id deriving (Show)

type Grammar =  [Nonterminal]


type NonTerminalId = String
type Id = String
type Expression = String
type Type = String
pp_l :: Label (DDG.P4DDG.E P4Types.Expression) -> String
pp_l Epsilon = "e"
pp_l Empty = "empty"
pp_l (Terminal s) = s
pp_l (NonTerminalCall n e) = n ++ "(" ++ e ++ ")"
-- pp_l (Bindings b) = intercalate "\n" $ map pp_binding b
pp_l (Statements s) = intercalate "\n" s

pp_l (Constraint e) = "[" ++ (pp e) ++ "]"

pp_binding (t, n, e) = 
    t ++ " "++ n ++ ":=" ++ e

ppr ::  Rule (DDG.P4DDG.E P4Types.Expression)  -> String
ppr (KleineClosure r) = "(" ++ ppr r ++ ")" ++ "*"
ppr (Alternation r1 r2) = "(" ++ "(" ++ ppr r1 ++ ")" ++ "|" ++ "(" ++ ppr r2 ++ ")"++ ")"
ppr (Sequence r1 r2) = "(" ++ ppr r1 ++ ")" ++ "(" ++ ppr r2 ++ ")"
ppr (Label l) = pp_l l

pp_Grammar :: Grammar -> String
pp_Grammar = unlines . map pp_nonterminal
    where
        pp_nonterminal (Nonterminal n [] r) = n ++ " = " ++ ppr r
        pp_nonterminal (Nonterminal n params r) = n ++ "(" ++ pp_params  params++ ") = " ++ ppr r
pp_params :: [Param] -> String
pp_params params = intercalate ", " $ map print params 
    where 
        print (Param t id) = t ++ " " ++ id 

prettifyDDG :: Grammar -> String
prettifyDDG grammar = intercalate "\n\n" (map prettifyNonterminal grammar)
  where
    prettifyNonterminal :: Nonterminal -> String
    prettifyNonterminal (Nonterminal ntId params rule) = 
      ntId ++ "(" ++ intercalate ", " (map prettifyParam params) ++ ") ::= \n  " ++ 
      indent 2 (prettifyRule rule)
    
    prettifyParam :: Param -> String
    prettifyParam (Param typ ident) = show typ ++ " " ++ ident
    
    indent :: Int -> String -> String
    indent n str = unlines (map (replicate n ' ' ++) (lines str))

prettifyRule :: Rule (E P4Types.Expression) -> String
prettifyRule = go 0
  where
    go :: Int -> Rule (E P4Types.Expression) -> String
    go indent rule = case rule of
      KleineClosure r -> 
        "(" ++ go indent r ++ ")*"
      
      Alternation r1 r2 -> 
        "(" ++ go (indent + 1) r1 ++ ")" ++ "\n" ++ replicate indent ' ' ++ "| " ++ "(" ++ go (indent + 1) r2 ++ ")"
      
      Sequence r1 r2 -> 
        go indent r1 ++ "\n" ++ go indent r2
      
      Label label -> prettyLabel indent label
    
    prettyLabel :: Int -> Label (E P4Types.Expression) -> String
    prettyLabel indent label = case label of
      Epsilon -> "ε"
      Empty -> "∅"
      Terminal str -> "\"" ++ str ++ "\""
      NonTerminalCall ntId str -> ntId ++ "(" ++ str ++ ")"
      Statements stmts -> "{" ++ intercalate "; " stmts ++ "}"
      Constraint e -> "[" ++ DDG.P4DDG.pp e ++ "]"