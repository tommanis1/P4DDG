type ParserDef = [Decl]

data Decl = 
    NonTerminal String [Stmt]
    | ParamterizedNonTerminal String [Param] [Stmt]

type Param = String

data Stmt = 
    Constrained Constraint Stmt
    | Assign String Expression
    | ExpressionStmt Expression
    | Field String
    | Type String

data Regex = 
    Plus Regex
    | List [Regex]
    | Star Regex
    | Terminal String

-- TODO pattern match might be nice
data Expression = 
    HostExpression String | Id String | Call String [Expression]

-- Host language expression can include "[" and "]", they easiest solution is to use some other character 
-- parse_Constrained = 
    -- symbol "\[" 

-- parsing the constraint itself could be nothing more than a string, but it would be nice from a user-perspective to differentiate between parser-defined variables and host language variables to provide meaningful error messages instead of generating P4 code that might not work 


-- TODO
type Constraint = String