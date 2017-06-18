import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr

data SL = WithPre  [Expression] Condition
        | WithPost [Expression] Condition
        | WithBoth [Expression] Condition Condition
        | Without  [Expression]
    deriving (Show)

data Expression = Constant Double
                | Identifier String
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | Modulus Expression Expression
                | Negation Expression
                | Cycle Condition [Expression]
                | CycleInv Condition Condition [Expression]
                | Conditional Condition [Expression] [Expression]
                | AssignmentStatement String Expression
                | DeclarationStatement String Expression
                deriving (Show)

data Condition = And Condition Condition
               | Or Condition Condition
               | Not Condition
               | Equal Expression Expression
               | LessThan Expression Expression
               | LessThanEqual Expression Expression
               deriving (Show)

lexer :: TokenParser ()
lexer = makeTokenParser (javaStyle { opStart  = oneOf "+-*/%|&=!<>¬"
                                   , opLetter = oneOf "+-*/%|&=!<>¬" 
                                   , reservedNames = [ "int", "while", "if", "then", "else", "pre", "postn", "poste" ]})

parseNumber :: Parser Expression
parseNumber = do
  val <- naturalOrFloat lexer
  case val of
    Left i -> return $ Constant $ fromIntegral i
    Right n -> return $ Constant $ n

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

parseCycle :: Parser Expression
parseCycle = do
    reserved lexer "while"
    c <- parseCondition
    braces lexer $ parseInnerCycle c

parseInnerCycle :: Condition -> Parser Expression
parseInnerCycle c = f <$> (try parseCondition) <*> many parseCommand
                <|> g <$> many parseCommand
  where f a b = CycleInv c a b
        g a   = Cycle c a

parseConditional :: Parser Expression
parseConditional = (try parseConditionalTwo) <|> parseConditionalOne

parseConditionalOne :: Parser Expression
parseConditionalOne = do
  reserved lexer "if"
  c <- parens lexer parseCondition
  reserved lexer "then"
  e1 <- braces lexer $ many parseCommand
  return $ Conditional c e1 []

parseConditionalTwo :: Parser Expression
parseConditionalTwo = do
  reserved lexer "if"
  c <- parens lexer parseCondition
  reserved lexer "then"
  e1 <- braces lexer $ many parseCommand
  reserved lexer "else"
  e2 <- braces lexer $ many parseCommand
  return $ Conditional c e1 e2

parseCondition :: Parser Condition
parseCondition = (flip buildExpressionParser) parseConditionalTerm $ [
    [ Prefix (reservedOp lexer "¬" >> return Not) ]
  , [ Infix  (reservedOp lexer "&&" >> return And) AssocLeft
    , Infix  (reservedOp lexer "||" >> return Or) AssocLeft ]
  ]

parseConditionalTerm :: Parser Condition
parseConditionalTerm =
  parens lexer parseCondition
  <|> parseComparison

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

parseTerm :: Parser Expression
parseTerm =
  parens lexer parseExpression 
  <|> parseNumber
  <|> (identifier lexer >>= return . Identifier)

parseDeclaration :: Parser Expression
parseDeclaration = do
  reserved lexer "int"
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseExpression
  semi lexer
  return $ DeclarationStatement ident expr

parseAssignment :: Parser Expression
parseAssignment = do
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseExpression
  semi lexer
  return $ AssignmentStatement ident expr

parseCommand :: Parser Expression
parseCommand = parseAssignment <|> parseConditional <|> parseCycle

parsePreCondition :: Parser Condition
parsePreCondition = do
    reserved lexer "pre"
    parseCondition

parsePosCondition :: Parser Condition
parsePosCondition = do
    reserved lexer "postn"
    parseCondition

parseSL :: Parser SL
parseSL = do
  whiteSpace lexer
  p  <- optionMaybe parsePreCondition
  s  <- many parseDeclaration
  q  <- many parseCommand
  p' <- optionMaybe parsePosCondition
  eof
  return $ case (p,p') of
    (Nothing,Nothing) -> Without (s++q)
    (Nothing,Just y)  -> WithPost (s++q) y
    (Just x,Nothing)  -> WithPre (s++q) x
    (Just x,Just y)   -> WithBoth (s++q) x y

main :: IO ()
main = getContents >>= parseTest parseSL
