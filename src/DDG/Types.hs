{-# LANGUAGE ExistentialQuantification #-}

module DDG.Types where
import DDG.P4DDG
import P4Types
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
pp_l :: (Show e) => Label e -> String
pp_l Epsilon = "e"
pp_l Empty = "empty"
pp_l (Terminal s) = s
pp_l (NonTerminalCall n e) = n ++ "(" ++ e ++ ")"
-- pp_l (Bindings b) = intercalate "\n" $ map pp_binding b
pp_l (Statements s) = intercalate "\n" s

pp_l (Constraint e) = "[" ++ (show e) ++ "]"

pp_binding (t, n, e) = 
    t ++ " "++ n ++ ":=" ++ e

pp :: (Show e) => Rule e  -> String
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


-- pp_Grammar = ... 