module PaperExampleGrammars where

import Grammar
{- 
int(n) = ([n > 0] (0 | . . . | 9)  {n:=n − 1})∗ [n = 0]
-}
-- a = [Nonterminal "int" ["n"] (
--     (KleineClosure (((RuleLabel $ Constrain "n > 0") `Sequence` (RuleLabel $ Terminal "0-9")) 
--     `Sequence` (RuleLabel $ Binding "n" "n-1"))) `Sequence`


-- )

-- (Sequence (Sequence (Constrain "n > 0") (Terminal "0-9")))]

{- 
dig = 0 | 1 | . . . | 9  
int(n) = [n = 0] | [n > 0] dig int(n − 1)-}
bb = 
    [
        Nonterminal "dig" [] (RuleLabel $ Terminal $ "0-9")
        
    ]

b = 
    [

        Nonterminal "int" ["n"] (
       Alternation (RuleLabel $ Constraint "n = 0")
         (Sequence (RuleLabel $ Constraint "n > 0") (Sequence (RuleLabel $ NonTerminalCall "dig" "") (RuleLabel $ NonTerminalCall "int" "n-1")))
    )
        ,    Nonterminal "dig" [] (RuleLabel $ Terminal $ "0-9")
        
    ]

{- 
A = (B ?) | (C !) B = x + x  C = (x + x) | (x - x)
-}
-- c = 

