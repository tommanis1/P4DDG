{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import qualified Data.Text as T
import Data.List
import Data.Char
import Data.Text (Text)
import Data.Void
import Data.Maybe
import Text.Megaparsec hiding (State, Token, label, (<?>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Internal (ParsecT(..))
import qualified Data.Set as Set
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import DDG.Types
import DDG.P4DDG

import P4Types
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec (parseTest)
prettyError :: (VisualStream s, ShowErrorComponent e, TraversableStream s) =>
               ParseErrorBundle s e -> String
prettyError = errorBundlePretty

data ErrorWithLabel = ErrorWithLabel String (ParseError String ErrorWithLabel)
  deriving (Eq, Ord)


instance ShowErrorComponent ErrorWithLabel where
  showErrorComponent (ErrorWithLabel l e) =
    "while parsing " <> l <> ",\n" <> parseErrorTextPretty e

instance Show ErrorWithLabel where
  show (ErrorWithLabel l e) = "while parsing " <> l <> ",\n" <> parseErrorPretty e

deriving instance Ord (ParseError String ErrorWithLabel)

type Parser = Parsec ErrorWithLabel String

label :: String -> Parser p -> Parser p
label l p = ParsecT $ \s cok cerr eeok eerr ->
  let addLabel e = FancyError (errorOffset e) .
        Set.singleton . ErrorCustom $ ErrorWithLabel l e
  in unParser (P.label l p) s cok (cerr . addLabel) eeok eerr

infix 0 <?>
(<?>) :: Parser p -> String -> Parser p
(<?>) = flip label

sc :: Parser ()
sc = L.space space1 (L.skipLineComment  "//") (L.skipBlockComment ( "/*") ( "*/"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parse_Grammar :: Parser Grammar
parse_Grammar = many parse_Nonterminal

parse_Nonterminal :: Parser Nonterminal
parse_Nonterminal = Nonterminal <$> 
    parse_StringStartingWithUppercase 
    <*> parse_params <*> (symbol "=" *> parse_Rule) <* symbol "$"

parse_StringStartingWithUppercase :: Parser String
parse_StringStartingWithUppercase = lexeme $ do
  first <- satisfy isUpper
  rest <- many (satisfy isAlphaNum <|> char '_')
  return (first : rest)

parse_params :: Parser [Param]
parse_params = choice [
    try $ parseParenParams  -- Try to parse (typ1 param1, type2 param2, ...)
    , pure []                -- Return empty list if no params
    ]
parseParenParams = between (symbol "(") (symbol ")") $ parse_p

parse_p = sepBy parseTypeAndVar (symbol ",")

parseTypeAndVar :: Parser Param
parseTypeAndVar = do
    paramType <- parse_type
    -- space1
    paramName <- parse_stringg
    return (Param paramType paramName)
parse_stringg = some (satisfy (\c -> isAlphaNum c || c == '_'))

parse_string = terminalChar --some (satisfy (\c -> isAlphaNum c || c == '_'))


-- parse_Rule = pure $ DDG.Grammar.Label Epsilon


-- parse_DDG.Grammar.Label :: 
-- parse_DDG.Grammar.Label = DDG.Grammar.Label <$> parse_Label

type Label = DDG.Types.Label (DDG.P4DDG.E P4Types.Expression)
type Rule = DDG.Types.Rule (DDG.P4DDG.E P4Types.Expression)
parse_Label :: Parser Parser.Label
parse_Label = choice [
    parse_Constraint
    , try $ parse_statements --parse_Bindings
    , try $ parse_Call
    , try $ parse_Terminal
    ] <?> "parse_Label"
parse_Constraint :: Parser Parser.Label
parse_Constraint = 
    Constraint <$>( symbol "[" *> parse_constraint_expression <* symbol "]") <?> "parse_Constraint"
parse_Call :: Parser Parser.Label
parse_Call = NonTerminalCall 
  <$> (parse_StringStartingWithUppercase <* symbol "(") 
  <*> parse_args 
  <* symbol ")" <?> "parse_Call"
-- parse_empty :: Parser (Label P4Types.Expression)
-- parse_empty = pure Empty<?> "parse_empty"
parse_empty :: Parser Parser.Rule
parse_empty =  do 
  _ <- symbol "()" 
  return (DDG.Types.Label Epsilon) 
  
  <?> "parse_empty"

--  do 
--   _ <- symbol "()" 
--   return Epsilon
  
--   <?> "parse_empty"
--  choice [
--     try $ (parse_StringStartingWithUppercase <* symbol "(") <*> parse_args  <* symbol ")"
--     -- , (parse_StringStartingWithUppercase <* symbol "(*" ) <*> (parse_any <* symbol "*)")
--     ]
parse_constraint_expression :: Parser (DDG.P4DDG.E P4Types.Expression)
parse_constraint_expression = do
    e1 <- parse_left_e
    op <- parse_expression_op
    e2 <- parse_right_e
    (return $ DDG.P4DDG.E $ P4Types.Expression{P4Types.self = e1 ++ " " ++ op ++ " " ++ e2, exprType = Nothing}) <?> "parse_constraint_expression"
    where 
        parse_expression_op = 
            choice [
                symbol "=="
                , symbol "!="
                ]

parse_left_e :: Parser String
parse_left_e = do
    parts <- some $ choice [
        terminalChar,
        symbol "+",
        symbol "-",
        symbol "*",
        symbol "/",
        -- symbol "%",
        symbol ">=",
        symbol "<=",
        symbol ">",
        symbol "<",
        symbol "&&",
        symbol "||",
        -- symbol "!",
        symbol ".",
        symbol ","
        , numberLiteral
        ]
    return $ concat parts
      --  <?> "parse_left_e"

parse_right_e :: Parser String 
parse_right_e = choice [
  try $ parse_tuple,
  try $ parse_args,
  terminalChar
  ]

parse_tuple :: Parser String
parse_tuple = do
  _ <- symbol "("
  elements <- sepBy parse_formatted_number (symbol ",")
  _ <- symbol ")"
  return $ "(" ++ intercalate ", " elements ++ ")"

parse_formatted_number :: Parser String
parse_formatted_number = lexeme $ do
  num <- some digitChar
  suffix <- many (alphaNumChar)
  return $ num ++ suffix

parse_args :: Parser String
parse_args = do
    args <-  sepBy expr_term sc
    return $ intercalate " " args

expr_term :: Parser String
expr_term = choice [
    symbol "+",
    symbol "-",
    symbol "*",
    symbol "/",
    -- symbol "%",
    symbol "==",
    symbol "!=",
    symbol ">=",
    symbol "<=",
    symbol ">",
    symbol "<",
    try $ symbol "&&&",
    symbol "&&",

    symbol "||",
    -- symbol "!",
    -- symbol "(",
    -- symbol ")",
    symbol ".",
    symbol ","
      , terminalChar

    , numberLiteral

    ] <?> "expr_term"

numberLiteral :: Parser String
numberLiteral = lexeme $ do
    d <- some digitChar
    return d

-- parse_Bindings :: Parser (Label P4Types.Expression)
-- parse_Bindings = Bindings <$>
--     (symbol "{" *> some parse_Binding) <* symbol "}" <?> "parse_Bindings"
    -- bindings <- some parse_Binding
-- parse_Binding = choice [
--   try parse_BindingWithType
--   , parse_BindingNoType
--   ]
parse_BindingWithType = do
    typ <- parse_Type
    ident <- parse_Id
    _ <- symbol "="
    expr <- parse_Expression
    _ <- symbol ";"
    return (typ, ident, expr)

parse_BindingNoType :: Parser (Type, Id, String)
parse_BindingNoType = do
    ident <- parse_Id
    _ <- symbol "="
    expr <- parse_Expression
    _ <- symbol ";"
    return ("", ident, expr)
    -- (parse_string <* symbol "=") <*> (parse_string <* symbol ";")

parse_statements :: Parser Parser.Label
parse_statements = do
    _ <- symbol "{"
    statements <- many (parse_statement)
    _ <- symbol "}"
    return $ Statements (map (\x -> x ++ ";") statements)
    
parse_statement :: Parser String
parse_statement = do
    words <- many parse_word
    _ <- symbol ";"
    return (unwords words)
  
parse_word :: Parser String
parse_word = lexeme $ some (satisfy isWordChar)
  where
    isWordChar c = isAlphaNum c || c `elem` "_-()[]{}+=*/.,<>!&|" && c /= ';' && c /= '}'
parse_Type = choice 
  [try $ parse_type
  , parse_string ]

parse_type = do
  s <-parse_string
  symbol "<"
  s2 <- parse_string
  symbol ">"
  return $ s ++ "<" ++ s2 ++ ">"
parse_Id = parse_string 
parse_Expression = parse_string
parse_Terminal :: Parser Parser.Label
parse_Terminal = Terminal <$> terminalChar <?> "parse_Terminal"

terminalChar :: Parser String
terminalChar = lexeme $ do
  str <- some (satisfy (\c -> isAlphaNum c || c == '_' || c == '-' || c == '.'))
  if str `elem` ["if", "else", "case", "of", "then"]
    then fail $ "Keyword " ++ show str ++ " is not allowed as a terminal"
    else return str


parse_Rule :: Parser Parser.Rule
parse_Rule = parse_Alternation <?> "parse_Rule"

parse_Alternation :: Parser Parser.Rule
parse_Alternation = do
  left <- parse_Sequence
  option left $ do
    symbol "|"
    -- Allow for an empty right side
    right <- option (DDG.Types.Label Empty) parse_Alternation
    return (Alternation left right)

parse_Sequence :: Parser Parser.Rule
parse_Sequence = do
  terms <- some parse_Term
  (return $ foldl1 Sequence terms) <?> "parse_Sequence"

parse_Term :: Parser Parser.Rule
parse_Term = choice [
    try parse_KleineClosure,
    try $ parse_If,
    try $ parse_Case,
    parse_Factor
  ] <?> "parse_Term"

parse_Factor :: Parser Parser.Rule
parse_Factor = choice [
    try $ parse_empty

    ,between (symbol "(") (symbol ")") parse_Rule,
    DDG.Types.Label <$> parse_Label
  ] <?> "parse_Factor"

parse_KleineClosure :: Parser Parser.Rule
parse_KleineClosure = do
  factor <- parse_Factor
  symbol "*"
  return (KleineClosure factor)


parse_If :: Parser Parser.Rule
parse_If = do
  symbol "if"
  symbol "["
  cond <- parse_constraint_expression
  symbol "]"
  symbol "then"
  thenBranch <- parse_Rule
  elseBranch <- optional $ do
    symbol "else"
    parse_Rule
  if (isNothing elseBranch)
    then return $ Sequence (DDG.Types.Label $ Constraint cond) (thenBranch)
    else
        do
          let if_ = Sequence (DDG.Types.Label $ Constraint cond) (thenBranch)
          let else_ =  Sequence (DDG.Types.Label $ Constraint (Not $ cond)) (fromJust elseBranch)
          return $ Alternation if_ else_ 
  <?> "parse_If"

parse_Case :: Parser Parser.Rule
parse_Case = do
  symbol "case"
  expr <- parse_any_expression_until "of"
  symbol "of"
  branches <- many parse_CaseBranch
  (return $ desugarCase expr branches) <?> "parse_Case"
  

parse_CaseBranch :: Parser ((DDG.P4DDG.E P4Types.Expression), Parser.Rule)
parse_CaseBranch = do
  symbol "%"
  patt <- parse_any_expression_until "->"
  lexeme $ string "->"
  rule <- parse_Rule
  symbol "%"
  return (patt, rule) <?> "parse_CaseBranch"
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

desugarCase :: DDG.P4DDG.E P4Types.Expression -> [((DDG.P4DDG.E P4Types.Expression), Parser.Rule)] -> Parser.Rule
desugarCase expr [(pattern, rule)] = 
  let condition = makeEqualityCondition expr pattern in
  if (strip $ P4Types.self . DDG.P4DDG.underlying $ pattern) == "_" then
    -- If the pattern is "_", treat it as a catch-all case
    rule
  else
    Sequence (DDG.Types.Label $ Constraint condition) rule
desugarCase expr ((pattern, rule):rest) = 
  if (strip $ P4Types.self . DDG.P4DDG.underlying $ pattern) == "_" then
    -- If the pattern is "_", treat it as a catch-all case
    let remainingBranches = desugarCase expr rest
    in 
      error $ show $ (P4Types.self . DDG.P4DDG.underlying $ pattern) == "_" ++ (P4Types.self . DDG.P4DDG.underlying $ pattern) ++ "test"
      --Alternation rule remainingBranches
  else
  -- Create equality condition between case expression and pattern
  let condition = makeEqualityCondition expr pattern
      -- Handle this branch
      thisBranch = Sequence (DDG.Types.Label $ Constraint condition) rule
      -- Recursively handle remaining branches
      remainingBranches = desugarCase expr rest
  in Alternation thisBranch remainingBranches

makeEqualityCondition :: DDG.P4DDG.E P4Types.Expression -> DDG.P4DDG.E P4Types.Expression -> DDG.P4DDG.E P4Types.Expression
makeEqualityCondition 
  (E e1) (E e2) =
    E $ P4Types.Expression{P4Types.self = (self e1) ++ " == " ++ (self e2), exprType = Nothing}

parse_any_expression :: Parser (DDG.P4DDG.E P4Types.Expression)
parse_any_expression = do
  -- Consume all tokens until we see a closing bracket
  content <- manyTill anySingle (lookAhead (char ']'))
  return $ DDG.P4DDG.E $ P4Types.Expression{P4Types.self = content, exprType = Nothing}
  <?> "parse_any_expression"

parse_any_expression_until :: String -> Parser (DDG.P4DDG.E P4Types.Expression)
parse_any_expression_until endToken = do
  -- Consume all tokens until we see the specified end token
  content <- manyTill anySingle (lookAhead (string endToken))
  -- Return the expression with the collected content
  return $ DDG.P4DDG.E $ P4Types.Expression{P4Types.self = content, exprType = Nothing}
  <?> "parse_any_expression"