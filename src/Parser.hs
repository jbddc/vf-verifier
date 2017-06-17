module Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec
import System.Environment

data SimpleLanguage = SL [Declaration] [Instruction]
    deriving (Show)
    
data Declaration = D String Integer
    deriving (Show)
    
data Instruction = 
      A String Expression
    | C1 RelationalExpression [Instruction]
    | C2 RelationalExpression [Instruction] [Instruction]
    | C RelationalExpression [Instruction]
    deriving (Show)

data RelationalExpression = 
      RE Expression RelationalOperator Expression
    | E0 Expression
    deriving (Show)

data RelationalOperator = EQ | NE | LT | LE | GT | GE
    deriving (Show)

data Expression = 
      E Term ExpressionOperator Expression
    | T0 Term
    deriving (Show)

data ExpressionOperator = ADD | SUB | OR
    deriving (Show)

data Term = 
      T Factor TermOperator Term 
    | F Factor
    deriving (Show)

data TermOperator = MUL | DIV | MOD | AND
    deriving (Show)

data Factor = 
      P RelationalExpression 
    | NOT RelationalExpression 
    | CONST Integer
    | VAR String
    deriving (Show)

parseSL :: Parser SimpleLanguage
parseSL = do
    declarations <- many parseDeclaration
    instructions <- many parseInstruction
    return $ Parser.SL declarations instructions

parseDeclaration :: Parser Declaration
parseDeclaration = do
    string "int"
    name <- many1 alphaNum
    char '='
    number <- read <$> many1 digit
    char ';'
    return $ Parser.D name number

parseInstruction :: Parser Instruction
parseInstruction = 
        parseAtribution 
    <|> parseCondicionalOne 
    <|> parseCondicionalTwo 
    <|> parseCycle

parseAtribution :: Parser Instruction
parseAtribution = do
    name <- many1 alphaNum
    char '='
    expression <- parseExpression
    char ';'
    return $ Parser.A name expression

parseCondicionalOne :: Parser Instruction
parseCondicionalOne = do
    string "if"
    char '('
    relationalExpression <- parseRelationalExpression
    char ')'
    char '{'
    instructions <- many1 parseInstruction
    char '}'
    return $ Parser.C1 relationalExpression instructions

parseCondicionalTwo :: Parser Instruction
parseCondicionalTwo = do
    string "if"
    char '('
    relationalExpression <- parseRelationalExpression
    char ')'
    char '{'
    instructionsOne <- many1 parseInstruction
    char '}'
    string "then"
    char '{'
    instructionsTwo <- many1 parseInstruction
    char '}'
    return $ Parser.C2 relationalExpression instructionsOne instructionsTwo

parseCycle :: Parser Instruction
parseCycle = do
    string "while"
    char '('
    relationalExpression <- parseRelationalExpression
    char ')'
    char '{'
    instructions <- many1 parseInstruction
    char '}'
    return $ Parser.C relationalExpression instructions

parseRelationalExpression :: Parser RelationalExpression
parseRelationalExpression = 
        f <$> parseExpression <*> parseRelationalOperator <*> parseExpression
    <|> g <$> parseExpression
    where
        f e1 ro e2 = Parser.RE e1 ro e2
        g e        = Parser.E0 e

parseRelationalOperator :: Parser RelationalOperator
parseRelationalOperator = 
        f <$> char '=' <*> char '='
    <|> g <$> char '!' <*> char '='
    <|> j <$> char '<'
    <|> h <$> char '<' <*> char '='
    <|> r <$> char '>'
    <|> s <$> char '>' <*> char '='
    where 
        f _ _ = Parser.EQ
        g _ _ = Parser.NE
        j _   = Parser.LT
        h _ _ = Parser.LE
        r _   = Parser.GT
        s _ _ = Parser.GE

parseExpression :: Parser Expression
parseExpression = 
        f <$> parseTerm <*> parseExpressionOperator <*> parseExpression
    <|> g <$> parseTerm
    where
        f t eo e = Parser.E t eo e
        g t      = Parser.T0 t

parseExpressionOperator :: Parser ExpressionOperator
parseExpressionOperator =
        f <$> char '+'
    <|> g <$> char '-'
    <|> j <$> char '|' <*> char '|'
    where 
        f _   = Parser.ADD
        g _   = Parser.SUB
        j _ _ = Parser.OR

parseTerm :: Parser Term
parseTerm = 
        g <$> parseFactor <*> parseTermOperator <*> parseTerm
    <|> h <$> parseFactor
    where
        g f to t = Parser.T f to t
        h f      = Parser.F f

parseTermOperator :: Parser TermOperator
parseTermOperator = 
        f <$> char '*'
    <|> g <$> char '/'
    <|> h <$> char '%'
    <|> j <$> char '&' <*> char '&'
    where 
        f _   = Parser.MUL
        g _   = Parser.DIV
        h _   = Parser.MOD
        j _ _ = Parser.AND

parseFactor :: Parser Factor
parseFactor = 
        f <$> char '(' <*> parseRelationalExpression <*> char ')'
    <|> g <$> char '!' <*> parseRelationalExpression
    <|> j <$> many1 digit
    <|> h <$> char '-' <*> many1 digit
    <|> w <$> many1 alphaNum
    where
        f _ re _ = Parser.P re
        g _ re   = Parser.NOT re
        j ds     = Parser.CONST . read $ ds
        h s ds   = Parser.CONST . read $ (s : ds)
        w v      = Parser.VAR v

main :: IO ()
main = getContents >>= parseTest parseSL

