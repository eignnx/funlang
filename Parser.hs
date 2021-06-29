module Parser where

import           Control.Monad
import           System.IO
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Language
import qualified Text.Parsec.Token as Token

type Ast = [Item]

data Item = Def String [String] Expr
  deriving Show

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
  | Block [Stmt]
  | Call Expr [Expr]
  | Intrinsic SourcePos String [Expr]
  deriving (Show)

data Stmt
  = Assign String Expr
  | Ret Expr
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
  , Token.identLetter     = alphaNum <|> oneOf "-"
  , Token.reservedNames   = [ "def"
                            , "if"
                            , "then"
                            , "else"
                            , "while"
                            , "do"
                            , "end"
                            , "intr"
                            , "nop"
                            , "true"
                            , "false"
                            , "ret"
                            , "not"
                            , "and"
                            , "or"
                            , "xor"
                            ]
  , Token.opStart         = oneOf "+-*/=<>!^?&|"
  , Token.opLetter        = oneOf "+-*/=<>!$@%^?&|"
  , Token.reservedOpNames = [ "+"
                            , "-"
                            , "*"
                            , "/"
                            , "=" -- assignment
                            , "<"
                            , ">"
                            , "=="
                            , "!="
                            ]
  }

lexer = Token.makeTokenParser languageDef

reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer

parens = Token.parens lexer
brackets = Token.brackets lexer
braces = Token.braces lexer
integer = Token.integer lexer
stringLiteral = Token.stringLiteral lexer
identifier = Token.identifier lexer

symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer
dot = Token.dot lexer
semi = Token.semi lexer
comma = Token.comma lexer

-- Statements
-- The main statement parser
langParser :: Parser [Item]
langParser = whiteSpace >> many1 item

item :: Parser Item
item = def

-- def my-fn[arg1, arg2, ...]
--   ...expr...
-- end
def :: Parser Item
def = do
  reserved "def"
  name   <- identifier
  params <- brackets $ sepEndBy identifier (symbol ",")
  expr <- expression
  reserved "end"
  return $ Def name params expr

statement :: Parser Stmt
statement =
  ifStmt <|> whileStmt <|> skipStmt <|> try assignStmt <|> (Expr <$> expression)

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
assignStmt = Assign <$> identifier <*> (reservedOp "=" *> expression)

returnStmt :: Parser Stmt
returnStmt = Ret <$> (reserved "ret" *> expression)

operators =
  [ [ Prefix (reservedOp "-" >> return (Unary Neg))
    , Prefix (reserved "not" >> return (Unary Not))
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
  , [ Infix (reserved "and" >> return (Binary (BoolOp And))) AssocLeft
    , Infix (reserved "or" >> return (Binary (BoolOp Or)))   AssocLeft
    , Infix (reserved "xor" >> return (Binary (BoolOp Xor))) AssocLeft
    ]
  ]

-- NOTE: Function application syntax is left-recursive!
-- That's why we gotta split things up into `termFirst`
-- and `term`.
term :: Parser Expr
term =  try nestedCalls
    <|> termFirst

-- my-func[x, y][1, 2][a, b, c]
nestedCalls :: Parser Expr
nestedCalls = do
  head <- termFirst
  allArgs <- many1 arguments
  return $ foldl Call head allArgs -- Build up all the calls

termFirst :: Parser Expr
termFirst =  (Var <$> identifier)
         <|> intrinsicExpr
         <|> blockExpr
         <|> (Literal <$> literal)
         <|> parens expression

-- Expressions
expression :: Parser Expr
expression = buildExpressionParser operators term

intrinsicExpr :: Parser Expr
intrinsicExpr = do
  pos <- getPosition
  reserved "intr"
  dot
  name <- identifier
  args <- arguments
  return $ Intrinsic pos name args

callExpr :: Parser Expr
callExpr = Call <$> expression <*> arguments

arguments :: Parser [Expr]
arguments = brackets $ sepEndBy expression comma

literal :: Parser Lit
literal =
  (Int <$> integer) <|> (Bool <$> boolean) <|> (String <$> stringLiteral)
 where
  boolean =
    (reserved "true" >> return True) <|> (reserved "false" >> return False)

blockExpr :: Parser Expr
blockExpr = Block <$> (reserved "do" *> sepEndBy statement semi <* reserved "end")

-- REPL Helper Functions
parseString :: String -> Ast
parseString = parseSrc "<string input>"

parseSrc :: String -> String -> Ast
parseSrc file src = case parse langParser file src of
  Left  e -> error $ show e
  Right r -> r

parseFile :: String -> IO Ast
parseFile file = do
  program <- readFile file
  case parse langParser file program of
    Left  e -> print e >> fail "parse error"
    Right r -> return r
