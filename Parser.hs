module Parser where

import           Control.Monad
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
  deriving (Show)

data BBinOp = And
            | Or
            | Xor
  deriving (Show)

data RBinOp = Gt
            | Lt
            | Eq
            | Neq
  deriving (Show)

-- Arithmetic Expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

data ABinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Skip
  | Intrinsic String [AExpr]
  deriving (Show)

languageDef = emptyDef
  { Token.commentStart    = "{#"
  , Token.commentEnd      = "#}"
  , Token.commentLine     = "#"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = [ "if"
                            , "then"
                            , "else"
                            , "while"
                            , "do"
                            , "end"
                            , "intr"
                            , "nop"
                            , "true"
                            , "false"
                            , "not"
                            , "and"
                            , "or"
                            , "xor"
                            ]
  , Token.reservedOpNames = [ "+"
                            , "-"
                            , "*"
                            , "/"
                            , "=" -- assignment
                            , "<"
                            , ">"
                            , "=="
                            , "!="
                            , "and"
                            , "or"
                            , "not"
                            , "xor"
                            ]
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

brackets = Token.brackets lexer

integer = Token.integer lexer

semiSep1 = Token.semiSep1 lexer

whiteSpace = Token.whiteSpace lexer

commaSep = Token.commaSep lexer

symbol = Token.symbol lexer

-- Statments
-- The main statement parser
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement <|> sequenceOfStmt

sequenceOfStmt = do
  list <- semiSep1 statement'
  return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt <|> intrStmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  stmt1 <- statement
  reserved "else"
  stmt2 <- statement
  reserved "end"
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpression
  reserved "do"
  stmt <- statement
  reserved "end"
  return $ While cond stmt

skipStmt :: Parser Stmt
skipStmt = reserved "nop" >> return Skip

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  reservedOp "="
  expr <- aExpression
  return $ Assign var expr

intrStmt :: Parser Stmt
intrStmt = do
  reserved "intr"
  symbol "."
  name <- identifier
  args <- brackets (commaSep aExpression)
  return $ Intrinsic name args


-- Expressions
aTerm :: Parser AExpr
aTerm = parens aExpression <|> fmap Var identifier <|> fmap IntConst integer

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

aOperators =
  [ [Prefix (reservedOp "-" >> return Neg)]
  , [ Infix (reservedOp "*" >> return (ABinary Mul)) AssocLeft
    , Infix (reservedOp "/" >> return (ABinary Div)) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (ABinary Add)) AssocLeft
    , Infix (reservedOp "-" >> return (ABinary Sub)) AssocLeft
    ]
  ]

bTerm :: Parser BExpr
bTerm =
  parens bExpression
    <|> (reserved "true" >> return (BoolConst True))
    <|> (reserved "false" >> return (BoolConst False))
    <|> rExpression

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

bOperators =
  [ [Prefix (reservedOp "not" >> return Not)]
  , [ Infix (reservedOp "and" >> return (BBinary And)) AssocLeft
    , Infix (reservedOp "or" >> return (BBinary Or))   AssocLeft
    , Infix (reservedOp "xor" >> return (BBinary Xor)) AssocLeft
    ]
  ]


-- Relational Operator Expressions
rExpression = do
  a1 <- aExpression
  op <- relation
  a2 <- aExpression
  return $ RBinary op a1 a2

relation =
  (reservedOp ">" >> return Gt)
    <|> (reservedOp "<" >> return Lt)
    <|> (reservedOp "==" >> return Eq)
    <|> (reservedOp "!=" >> return Neq)

-- REPL Helper Functions
parseString :: String -> Stmt
parseString str = case parse whileParser "<string input>" str of
  Left  e -> error $ show e
  Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  case parse whileParser file program of
    Left  e -> print e >> fail "parse error"
    Right r -> return r
