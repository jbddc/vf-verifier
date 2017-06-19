-- |
-- Module    : SLParser
-- Copyright : (c) João Costa, 2016-2017
--             (c) Rafael Barbosa, 2016-2017
-- License   : BSD3
-- Maintainer: João Costa <a70430@alunos.uminho.pt>,
--             Rafael Barbosa <a71580@alunos.uminho.pt>
--
-- A parser for the Simple Language lang using Parsec.
module SLParser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr

data SL 
  = WithPre  [Expression] Condition -- ^ AST Root with Pre-condition annotations
  | WithPost [Expression] Condition (Maybe Condition) -- ^ AST Root with Post-condition annotations
  | WithBoth [Expression] Condition Condition (Maybe Condition) -- ^ AST Root with Pre and Post condition annotations
  | Without  [Expression]  -- ^ AST Root without Annotations
    deriving (Show)

data Expression = Constant Integer -- ^ AST integer representation
                | Identifier String -- ^ AST variable identifier representation
                | Addition Expression Expression -- ^ AST representation of an addition between 2 expressions
                | Subtraction Expression Expression -- ^ AST representation of a subtraction between 2 expressions
                | Multiplication Expression Expression -- ^ AST representation of a multiplication between 2 expressions
                | Division Expression Expression -- ^ AST representation of a division between 2 expressions
                | Modulus Expression Expression -- ^ AST representation of an expression 'mod' another
                | Negation Expression -- ^ AST representation of the negation of an expression
                | Cycle Condition [Expression] -- ^ AST representation of a cycle (condition plus it's inner expressions)
                | CycleInv Condition Condition [Expression] -- ^ AST representation of a cycle (same as above) with invariant
                | Conditional Condition [Expression] [Expression] -- ^ AST representation of an if-then-else
                | AssignmentStatement String Expression -- ^ AST representation of an assignment. eg: x = 4;
                | DeclarationStatement String Expression -- ^ AST representation of a var declaration. eg: int x = 4;
                | Try [Expression] [Expression] -- ^ AST representation of a try-catch.
                | Throw -- ^ throw command AST representation.
                deriving (Show)

data Condition = And Condition Condition -- ^ AST representation of '&&'
               | Or Condition Condition -- ^ AST representation of '||'
               | Not Condition -- ^ AST representation of 'not'
               | Equal Expression Expression -- ^ AST representation of '=='
               | LessThan Expression Expression -- ^ AST representaiton of '<'
               | LessThanEqual Expression Expression -- ^ AST representation '<='
               | Boolean Bool -- ^ AST representation of 'True' / 'False' values
               deriving (Show)

-- | Define lexer language.
lexer :: TokenParser ()
lexer = makeTokenParser (javaStyle { opStart  = oneOf "+-*/%|&=!<>¬"
                                   , opLetter = oneOf "+-*/%|&=!<>¬" 
                                   , reservedNames = [ "int", "while", "if", "then", "else", "try", "catch", "throw", "pre", "postn", "poste" ]})

-- | Function that parses a number and converts it to `Constant` `Integer`.
parseNumber :: Parser Expression
parseNumber = do
  val <- integer lexer
  return $ Constant val

-- | Function that parses an expression to its given AST representation.
parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseTerm $ [
    [ Prefix (reservedOp lexer "-" >> return Negation) 
    , Prefix (reservedOp lexer "+" >> return id) ]
  , [ Infix  (reservedOp lexer "*" >> return Multiplication) AssocLeft
    , Infix  (reservedOp lexer "/" >> return Division) AssocLeft 
    , Infix  (reservedOp lexer "%" >> return Modulus) AssocLeft ]
  , [ Infix  (reservedOp lexer "+" >> return Addition) AssocLeft
    , Infix  (reservedOp lexer "-" >> return Subtraction) AssocLeft ]
  ]

-- | Function that parses a Cycle to its given AST representation.
parseCycle :: Parser Expression
parseCycle = do
    reserved lexer "while"
    c <- parseCondition
    braces lexer $ parseInnerCycle c

-- | `parseCycle` auxiliary function that parses code inside the cycle.
parseInnerCycle :: Condition -> Parser Expression
parseInnerCycle c = f <$> (brackets lexer $ try parseCondition) <*> many parseCommand
                <|> g <$> many parseCommand
  where f a b = CycleInv c a b
        g a   = Cycle c a

-- | Function that parses an if-then-else expression. Either with or without else branch.
parseConditional :: Parser Expression
parseConditional = (try parseConditionalTwo) <|> parseConditionalOne

-- | Function helper to `parseConditional` that parses if-then-else without else branch.
parseConditionalOne :: Parser Expression
parseConditionalOne = do
  reserved lexer "if"
  c <- parens lexer parseCondition
  reserved lexer "then"
  e1 <- braces lexer $ many parseCommand
  return $ Conditional c e1 []

-- | Function helper to `parseConditional` that parses if-then-else with else branch.
parseConditionalTwo :: Parser Expression
parseConditionalTwo = do
  reserved lexer "if"
  c <- parens lexer parseCondition
  reserved lexer "then"
  e1 <- braces lexer $ many parseCommand
  reserved lexer "else"
  e2 <- braces lexer $ many parseCommand
  return $ Conditional c e1 e2

-- | Function that parses a boolean expression.
parseCondition :: Parser Condition
parseCondition = (flip buildExpressionParser) parseConditionalTerm $ [
    [ Prefix (reservedOp lexer "¬" >> return Not) ]
  , [ Infix  (reservedOp lexer "&&" >> return And) AssocLeft
    , Infix  (reservedOp lexer "||" >> return Or) AssocLeft ]
  ]

-- | Function that parses a `True` / `False` boolean. 
parseBool :: Parser Condition
parseBool = f <$> reserved lexer "true"
  <|> g <$> reserved lexer "false"
  where f _ = Boolean True 
        g _ = Boolean False

-- | `parseConditional` function helper that sets operator priority. 
parseConditionalTerm :: Parser Condition
parseConditionalTerm =
  parens lexer parseCondition
  <|> parseBool
  <|> parseComparison

-- | `parseConditionalTerm` function helper that sets operator priority.
parseComparison :: Parser Condition
parseComparison = do
  e1 <- parseExpression
  f <- (reserved lexer "==" >> return (Equal e1))
       <|> (reserved lexer "<=" >> return (LessThanEqual e1))
       <|> (reserved lexer "<" >> return (LessThan e1))
       <|> (reserved lexer ">=" >> return (Not . (LessThan e1)))
       <|> (reserved lexer ">" >> return (Not . (LessThanEqual e1)))
       <|> (reserved lexer "!=" >> return (Not . (Equal e1)))
  e2 <- parseExpression
  return $ f e2

-- | Function helper that sets operator priority in expressions.
parseTerm :: Parser Expression
parseTerm =
  parens lexer parseExpression 
  <|> parseNumber
  <|> (identifier lexer >>= return . Identifier)

-- | Function that parses variable declarations.
parseDeclaration :: Parser Expression
parseDeclaration = do
  reserved lexer "int"
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseExpression
  semi lexer
  return $ DeclarationStatement ident expr

-- | Function that parses variable assignments.
parseAssignment :: Parser Expression
parseAssignment = do
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseExpression
  semi lexer
  return $ AssignmentStatement ident expr

-- | Top-level function that parses an expression
parseCommand :: Parser Expression
parseCommand = parseThrow <|> parseAssignment <|> parseConditional <|> parseCycle <|> parseTryCatch

-- | Function that parses throw command. 
parseThrow :: Parser Expression
parseThrow = try $ do
  reserved lexer "throw"
  semi lexer
  return $ Throw

-- | Function that parses pre condition annotations.
parsePreCondition :: Parser Condition
parsePreCondition = do
    reserved lexer "pre"
    parseCondition

-- | Function that parses normal post condition annotations.
parsePosNCondition :: Parser Condition
parsePosNCondition = do
    reserved lexer "postn"
    parseCondition

-- | Function that parses post condition annotations for exception events.
parsePosECondition :: Parser Condition
parsePosECondition = do
    reserved lexer "poste"
    parseCondition

-- | Function that parses try-catch enclosure.
parseTryCatch :: Parser Expression
parseTryCatch = do
  reserved lexer "try"
  tryCmds <- braces lexer $ many parseCommand
  reserved lexer "catch"
  catchCmds <- braces lexer $ many parseCommand
  return $ Try tryCmds catchCmds

-- | Function that bootstraps the parsing process, returning the corresponding AST.
parseSL :: Parser SL
parseSL = do
  whiteSpace lexer
  p  <- optionMaybe parsePreCondition
  s  <- many parseDeclaration
  q  <- many parseCommand
  p' <- optionMaybe parsePosNCondition
  pe <- optionMaybe parsePosECondition
  eof
  return $ case (p,p') of
    (Nothing,Nothing) -> Without (s++q)
    (Nothing,Just y)  -> WithPost (s++q) y pe
    (Just x,Nothing)  -> WithPre (s++q) x
    (Just x,Just y)   -> WithBoth (s++q) x y pe
