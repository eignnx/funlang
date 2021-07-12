{-# LANGUAGE BlockArguments #-}

module Parser
  ( parseString
  , parseSrc
  , parseFile
  )
where

import qualified Ast                  as Ast
import qualified Ty                   as Ty
import           Control.Monad
import           System.IO
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as Token
import           Debug.Trace          ( trace )

languageDef = emptyDef
  { Token.commentStart    = "{#"
  , Token.commentEnd      = "#}"
  , Token.commentLine     = "#"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "-"
  , Token.reservedNames   = [ "def"
                            , "let"
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
                            , "->"
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
colon = Token.colon lexer

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
-- ```
def :: Parser Ast.Item
def = do
  reserved "def"
  name       <- identifier
  let param  =  (,) <$> identifier <*> (colon *> ty)
  params     <- brackets $ sepEndBy param (symbol ",")
  let exprTail = (do reservedOp "="
                     body <- expression
                     return $ Ast.Def name params (body, Nothing))
  let blockTail = (do retTy <- option Ty.unitTy (reservedOp "->" *> ty) -- Without a ret ty, def defaults to returning Unit.
                      body  <- blockExpr
                      return $ Ast.Def name params (body, Just retTy))
  exprTail <|> blockTail

ty :: Parser Ty.Ty
ty = Ty.ValTy <$> identifier

ifExpr :: Parser Ast.Expr
ifExpr = do
  reserved "if"
  cond <- expression
  reserved "then"
  yesBody <- expression
  reserved "else"
  noBody <- expression
  reserved "end"
  return $ Ast.If cond yesBody noBody

whileExpr :: Parser Ast.Expr
whileExpr = do
  reserved "while"
  cond <- expression
  body <- expression
  return $ Ast.While cond body

nopExpr :: Parser Ast.Expr
nopExpr = reserved "nop" *> return Ast.Nop

letExpr :: Parser Ast.Expr
letExpr = Ast.Let <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> expression)

assignExpr :: Parser Ast.Expr
assignExpr = Ast.Assign <$> identifier <*> (reservedOp "=" *> expression)

returnExpr :: Parser Ast.Expr
returnExpr = Ast.Ret <$> (reserved "ret" *> expression)

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
    <|> try nestedAnn
    <|> termFirst

-- my-func[x, y][1, 2][a, b, c]
nestedCalls :: Parser Ast.Expr
nestedCalls = do
  head <- termFirst
  allArgs <- many1 arguments
  return $ foldl Ast.Call head allArgs -- Build up all the calls

-- 1 : Never : Nat : Int
nestedAnn :: Parser Ast.Expr
nestedAnn = do
  head <- termFirst
  allTys <- many1 (colon *> ty)
  return $ foldl Ast.Ann head allTys -- Build up all the calls

varExpr :: Parser Ast.Expr
varExpr = Ast.Var <$> identifier

literalExpr :: Parser Ast.Expr
literalExpr = Ast.Literal <$> literal

termFirst :: Parser Ast.Expr
termFirst =  semiEndedTerm <|> endEndedTerm


semiEndedTerm =  intrinsicExpr
             <|> parens expression
             <|> nopExpr
             <|> returnExpr
             <|> letExpr
             <|> literalExpr
             <|> try assignExpr
             <|> varExpr

endEndedTerm =  blockExpr
            <|> ifExpr
            <|> whileExpr

semiEndedExpr = try nestedCalls <|> semiEndedTerm 
endEndedExpr = endEndedTerm

-- Expressions
expression :: Parser Ast.Expr
expression = buildExpressionParser operators term

terminatedExpr :: Parser Ast.Expr
terminatedExpr = (semiEndedExpr <* semi) <|> endEndedExpr

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
blockExpr = do
  reserved "do"
  es <- many $ try terminatedExpr
  e <- optionMaybe expression
  reserved "end"
  return case e of
    Just expr -> Ast.Block Ast.NotVoid (es ++ [expr])
    Nothing | endsInEndTerminatedExpr es -> Ast.Block Ast.NotVoid es
    _ -> Ast.Block Ast.IsVoid es
  where
    endsInEndTerminatedExpr [] = False
    endsInEndTerminatedExpr (e:[]) = Ast.isEndTerminatedExpr e
    endsInEndTerminatedExpr (_:es) = endsInEndTerminatedExpr es

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
