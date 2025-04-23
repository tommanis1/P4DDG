module DDG.P4DDG where

data Stmt r e = 
    If e (Stmt r e) (Stmt r e)
    | Case e [(e, (Stmt r e))]
    | Rule r 
    deriving (Show, Eq, Ord)
    -- | Seq [(Stmt r e)]

data E e = Not (E e) | In e e | E e deriving (Show, Eq, Ord)