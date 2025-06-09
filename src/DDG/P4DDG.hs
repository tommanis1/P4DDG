{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DDG.P4DDG where
import P4Types(ppP4E, Expression(..))

data E e = Not (E e) | In e e | E e deriving (Show, Eq, Ord)

underlying :: E e -> e
underlying (E e) = e
underlying (Not e) = underlying e
-- underlying (In e1 e2) = e1


pp :: E P4Types.Expression -> String
pp (E e) = ppP4E e
pp (Not e) = "!" ++ DDG.P4DDG.pp e
-- pp (In e1 e2) = ppP4E e1 ++ " in " ++ ppP4E e2
