data Rule r l = 
    KleineClosure r
    | Alternation r r
    | Sequence r r
    | RuleLabel l deriving (Show, Eq)
 