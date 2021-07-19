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
import qualified Data.Char            as Char
import           System.FilePath      ( takeBaseName )

languageDef = emptyDef
  { Token.commentStart    = "{#"
  , Token.commentEnd      = "#}"
  , Token.commentLine     = "#"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "-"
  , Token.reservedNames   = [ "mod"
                            , "def"
                            , "let"
                            , "if"
                            , "then"
                            , "else"
                            , "while"
                            , "loop"
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
langParser :: Parser Ast.Expr
langParser = innerMod

-- Parses a file into a module. Definitions can be placed directly without
-- declaring the module with the `mod` keyword.
innerMod :: Parser Ast.Expr
innerMod = do
  items <- whiteSpace >> many expression
  return $ Ast.Mod "#file" items

modExpr :: Parser Ast.Expr
modExpr = do
  reserved "mod"
  name <- identifier
  items <- whiteSpace >> many expression
  reserved "end"
  return $ Ast.Mod name items

-- ```
-- def my-fn[arg1, arg2, ...] do
--   ...stmts...
-- end
-- ```
-- OR
-- ```
-- def my-fn[arg1, arg2, ...] = ...expr...
-- ```
defExpr :: Parser Ast.Expr
defExpr = do
  reserved "def"
  name       <- identifier
  let param  =  (,) <$> identifier <*> (colon *> ty)
  params     <- brackets $ sepEndBy param (symbol ",")
  let exprTail = (do reservedOp "="
                     body <- expression
                     return $ Ast.Def name params Nothing body)
  let blockTail = (do retTy <- option Ty.VoidTy (reservedOp "->" *> ty) -- Without a ret ty, def defaults to returning Unit.
                      body  <- blockExpr
                      return $ Ast.Def name params (Just retTy) body)
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

loopExpr :: Parser Ast.Expr
loopExpr = do
  reserved "loop"
  body <- expression
  return $ Ast.Loop body

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
            <|> loopExpr
            <|> defExpr
            <|> modExpr

endEndedExpr = endEndedTerm

-- Expressions
expression :: Parser Ast.Expr
expression = buildExpressionParser operators term

terminatedExpr :: Parser Ast.Expr
terminatedExpr = try (expression <* semi) <|> endEndedExpr

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

seqTerminatedBy :: Parser a -> Parser (Ast.Seq Ast.Expr)
seqTerminatedBy end
  -- First case: terminatedExpr IMMEDIATELY FOLLOWED BY end, should be put in
  -- `Ast.Result`, NOT `Ast.Semi _ Ast.End`. This allows `if` exprs to appear
  -- at the end of a block and return their result without looking like they
  -- return `Void`.
  =   try (Ast.Result <$> (terminatedExpr <* lookAhead end))
  <|> try (Ast.Semi <$> terminatedExpr <*> seqTerminatedBy end)
  <|> (Ast.Result <$> (expression <* lookAhead end))
  <|> (lookAhead end *> return Ast.Empty)

blockExpr :: Parser Ast.Expr
blockExpr = do
  reserved "do"
  seq <- seqTerminatedBy (reserved "end")
  reserved "end"
  return $ Ast.Block seq

-- REPL Helper Functions
parseString :: String -> Ast.Expr
parseString = parseSrc "<string input>"

parseSrc :: String -> String -> Ast.Expr
parseSrc file src = case parse langParser file src of
  Left  e -> error $ show e
  Right r -> r

parseFile :: String -> IO Ast.Expr
parseFile fileName = do
  src <- readFile fileName
  case parse langParser fileName src of
    Left  e -> print e >> fail "parse error"
    Right (Ast.Mod _ items) -> do
      let modName = modNameFromFileName fileName
      return $ Ast.Mod modName items

modNameFromFileName :: String -> String
modNameFromFileName fileName = capitalize $ takeBaseName fileName
  where capitalize (c:cs) = Char.toUpper c : cs