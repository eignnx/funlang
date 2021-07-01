module Parser
  ( parseString
  , parseSrc
  , parseFile
  )
where

import qualified Ast                  as Ast
import           Control.Monad
import           System.IO
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as Token

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
                            , "++"
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
langParser :: Parser Ast.Ast
langParser = whiteSpace >> many1 item

item :: Parser Ast.Item
item = def

-- ```
-- def my-fn[arg1, arg2, ...] do
--   ...stmts...
-- end
-- ```
-- OR
-- ```
-- def my-fn[arg1, arg2, ...] = ...expr...
def :: Parser Ast.Item
def = do
  reserved "def"
  name   <- identifier
  params <- brackets $ sepEndBy identifier (symbol ",")
  expr <- (reservedOp "=" *> expression) <|> blockExpr
  return $ Ast.Def name params expr

statement :: Parser Ast.Stmt
statement = ifStmt
         <|> whileStmt
         <|> skipStmt
         <|> returnStmt
         <|> try assignStmt
         <|> exprStmt

ifStmt :: Parser Ast.Stmt
ifStmt = do
  reserved "if"
  cond <- expression
  reserved "then"
  stmt1 <- (Ast.Expr . Ast.Block) <$> many statement
  reserved "else"
  stmt2 <- (Ast.Expr . Ast.Block) <$> many statement
  reserved "end"
  return $ Ast.If cond stmt1 stmt2

whileStmt :: Parser Ast.Stmt
whileStmt = do
  reserved "while"
  cond <- expression
  stmt <- Ast.Expr <$> blockExpr
  return $ Ast.While cond stmt

skipStmt :: Parser Ast.Stmt
skipStmt = reserved "nop" *> semi *> return Ast.Skip

assignStmt :: Parser Ast.Stmt
assignStmt = Ast.Assign <$> identifier <*> (reservedOp "=" *> expression) <* semi

returnStmt :: Parser Ast.Stmt
returnStmt = Ast.Ret <$> (reserved "ret" *> expression <* semi)

exprStmt :: Parser Ast.Stmt
exprStmt = Ast.Expr <$> expression <* semi

operators =
  [ [ Prefix (reservedOp "-" >> return (Ast.Unary Ast.Neg))
    , Prefix (reserved "not" >> return (Ast.Unary Ast.Not))
    ]
  , [ Infix (reservedOp "*" >> return (Ast.Binary (Ast.ArithOp Ast.Mul))) AssocLeft
    , Infix (reservedOp "/" >> return (Ast.Binary (Ast.ArithOp Ast.Div))) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Ast.Binary (Ast.ArithOp Ast.Add))) AssocLeft
    , Infix (reservedOp "-" >> return (Ast.Binary (Ast.ArithOp Ast.Sub))) AssocLeft
    , Infix (reservedOp "++" >> return (Ast.Binary (Ast.OtherOp Ast.Concat))) AssocLeft
    ]
  , [ Infix (reservedOp ">" >> return (Ast.Binary (Ast.RelOp Ast.Gt)))   AssocLeft
    , Infix (reservedOp "<" >> return (Ast.Binary (Ast.RelOp Ast.Lt)))   AssocLeft
    , Infix (reservedOp "==" >> return (Ast.Binary (Ast.RelOp Ast.Eq)))  AssocLeft
    , Infix (reservedOp "!=" >> return (Ast.Binary (Ast.RelOp Ast.Neq))) AssocLeft
    ]
  , [ Infix (reserved "and" >> return (Ast.Binary (Ast.BoolOp Ast.And))) AssocLeft
    , Infix (reserved "or" >> return (Ast.Binary (Ast.BoolOp Ast.Or)))   AssocLeft
    , Infix (reserved "xor" >> return (Ast.Binary (Ast.BoolOp Ast.Xor))) AssocLeft
    ]
  ]

-- NOTE: Function application syntax is left-recursive!
-- That's why we gotta split things up into `termFirst`
-- and `term`.
term :: Parser Ast.Expr
term =  try nestedCalls
    <|> termFirst

-- my-func[x, y][1, 2][a, b, c]
nestedCalls :: Parser Ast.Expr
nestedCalls = do
  head <- termFirst
  allArgs <- many1 arguments
  return $ foldl Ast.Call head allArgs -- Build up all the calls

termFirst :: Parser Ast.Expr
termFirst =  (Ast.Var <$> identifier)
         <|> intrinsicExpr
         <|> blockExpr
         <|> (Ast.Literal <$> literal)
         <|> parens expression

-- Expressions
expression :: Parser Ast.Expr
expression = buildExpressionParser operators term

intrinsicExpr :: Parser Ast.Expr
intrinsicExpr = do
  pos <- getPosition
  reserved "intr"
  dot
  name <- identifier
  args <- arguments
  return $ Ast.Intrinsic pos name args

callExpr :: Parser Ast.Expr
callExpr = Ast.Call <$> expression <*> arguments

arguments :: Parser [Ast.Expr]
arguments = brackets $ sepEndBy expression comma

literal :: Parser Ast.Lit
literal =
  (Ast.Int <$> integer) <|> (Ast.Bool <$> boolean) <|> (Ast.String <$> stringLiteral)
 where
  boolean =
    (reserved "true" >> return True) <|> (reserved "false" >> return False)

blockExpr :: Parser Ast.Expr
blockExpr = Ast.Block <$> (reserved "do" *> many statement <* reserved "end")

-- REPL Helper Functions
parseString :: String -> Ast.Ast
parseString = parseSrc "<string input>"

parseSrc :: String -> String -> Ast.Ast
parseSrc file src = case parse langParser file src of
  Left  e -> error $ show e
  Right r -> r

parseFile :: String -> IO Ast.Ast
parseFile file = do
  program <- readFile file
  case parse langParser file program of
    Left  e -> print e >> fail "parse error"
    Right r -> return r
