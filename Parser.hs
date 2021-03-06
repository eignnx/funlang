module Parser where

import           Control.Monad
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

data BinOp
  = ArithOp ArithOp
  | BoolOp BoolOp
  | RelOp RelOp
  deriving (Show)

data ArithOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

data BoolOp
  = And
  | Or
  | Xor
  deriving (Show)

data RelOp
  = Gt
  | Lt
  | Eq
  | Neq
  deriving (Show)

data Lit
  = Bool Bool
  | Int Integer
  | String String
  deriving (Show)

data UnaryOp
  = Not
  | Neg
  deriving (Show)

data Expr
  = Var String
  | Literal Lit
  | Unary UnaryOp Expr
  | Binary BinOp Expr Expr
  | Intrinsic SourcePos String [Expr]
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Skip
  | Expr Expr
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

braces = Token.braces lexer

integer = Token.integer lexer

semiSep1 = Token.semiSep1 lexer

whiteSpace = Token.whiteSpace lexer

commaSep = Token.commaSep lexer

symbol = Token.symbol lexer

stringLiteral = Token.stringLiteral lexer

-- Statments
-- The main statement parser
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = braces statement <|> sequenceOfStmt

sequenceOfStmt = do
  stmts <- many1 (statement' <* symbol ";")
  return $ if length stmts == 1 then head stmts else Seq stmts

statement' :: Parser Stmt
statement' =
  ifStmt <|> whileStmt <|> skipStmt <|> assignStmt <|> (Expr <$> expression)

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- expression
  reserved "then"
  stmt1 <- statement
  reserved "else"
  stmt2 <- statement
  reserved "end"
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- expression
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
  expr <- expression
  return $ Assign var expr

-- Expressions
expression :: Parser Expr
expression = buildExpressionParser arithOperators term

term :: Parser Expr
term =
  parens expression
    <|> (Var <$> identifier)
    <|> (Literal <$> literal)
    <|> intrinsicExpr

intrinsicExpr :: Parser Expr
intrinsicExpr = do
  pos <- getPosition
  symbol "@"
  name <- identifier
  args <- brackets (commaSep expression)
  return $ Intrinsic pos name args

literal :: Parser Lit
literal =
  (Int <$> integer) <|> (Bool <$> boolean) <|> (String <$> stringLiteral)
 where
  boolean =
    (reserved "true" >> return True) <|> (reserved "false" >> return False)


arithOperators =
  [ [ Prefix (reservedOp "-" >> return (Unary Neg))
    , Prefix (reservedOp "not" >> return (Unary Not))
    ]
  , [ Infix (reservedOp "*" >> return (Binary (ArithOp Mul))) AssocLeft
    , Infix (reservedOp "/" >> return (Binary (ArithOp Div))) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Binary (ArithOp Add))) AssocLeft
    , Infix (reservedOp "-" >> return (Binary (ArithOp Sub))) AssocLeft
    ]
  , [ Infix (reservedOp ">" >> return (Binary (RelOp Gt)))   AssocLeft
    , Infix (reservedOp "<" >> return (Binary (RelOp Lt)))   AssocLeft
    , Infix (reservedOp "==" >> return (Binary (RelOp Eq)))  AssocLeft
    , Infix (reservedOp "!=" >> return (Binary (RelOp Neq))) AssocLeft
    ]
  , [ Infix (reservedOp "and" >> return (Binary (BoolOp And))) AssocLeft
    , Infix (reservedOp "or" >> return (Binary (BoolOp Or)))   AssocLeft
    , Infix (reservedOp "xor" >> return (Binary (BoolOp Xor))) AssocLeft
    ]
  ]

-- REPL Helper Functions
parseString :: String -> Stmt
parseString str = parseSrc "<string input>" str

parseSrc :: String -> String -> Stmt
parseSrc file src = case parse whileParser file src of
  Left  e -> error $ show e
  Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  case parse whileParser file program of
    Left  e -> print e >> fail "parse error"
    Right r -> return r
