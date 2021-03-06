{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Parser
  ( parseString,
    parseSrc,
    parseFile,
  )
where

import qualified Ast
import Cata (At (..))
import Control.Monad
import qualified Data.Char as Char
import Data.Functor (($>))
import qualified Data.Functor.Identity
import Debug.Trace (trace)
import System.FilePath (takeBaseName)
import System.IO
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import qualified Ty
import Utils (Span (..), code, mkSpan, (+++))

languageDef =
  emptyDef
    { Token.commentStart = "{#",
      Token.commentEnd = "#}",
      Token.commentLine = "#",
      Token.nestedComments = True,
      Token.identStart = letter,
      Token.identLetter = alphaNum <|> oneOf "_",
      Token.reservedNames =
        [ "mod",
          "type",
          "rec",
          "def",
          "let",
          "const",
          "if",
          "then",
          "else",
          "match",
          "while",
          "loop",
          "do",
          "end",
          "intr",
          "nop",
          "true",
          "false",
          "ret",
          "not",
          "and",
          "or",
          "xor",
          "as"
        ],
      Token.opStart = oneOf "+-*/=<>!^?&|",
      Token.opLetter = oneOf "+-*/=<>!$@%^?&|",
      Token.reservedOpNames =
        [ "+",
          "-",
          "*",
          "/",
          "=", -- assignment
          "<",
          ">",
          -- , "=="
          -- , "!="
          "++",
          "->",
          "=>",
          "|"
        ]
    }

lexer = Token.makeTokenParser languageDef

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

brackets = Token.brackets lexer

braces = Token.braces lexer

integer = Token.integer lexer

natural = Token.natural lexer

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
innerMod = spanned do
  items <- whiteSpace >> many expression
  return $ Ast.ModF "#file" items

modExpr :: Parser Ast.Expr
modExpr = spanned do
  reserved "mod"
  name <- identifier
  items <- whiteSpace >> many expression
  reserved "end"
  return $ Ast.ModF name items

tyDefExpr :: Parser Ast.Expr
tyDefExpr = spanned do
  reserved "type"
  isRec <- option Ast.NotRec $ reserved "rec" $> Ast.IsRec
  name <- identifier
  ctorDefs <- many $ reservedOp "|" *> tyCmpntDef
  reserved "end"
  return $ Ast.TyDefF isRec name ctorDefs

tyCmpntDef :: Parser Ast.TyCmpntDef
tyCmpntDef = vrntDef <|> subTyDef
  where
    vrntDef = do
      colon
      name <- identifier
      components <- sepEndBy maybeRecTy comma
      return $ Ast.VrntDef (':' : name) components
    subTyDef = Ast.SubTyDef <$> identifier
    maybeRecTy =
      try (Ast.NonRecTy <$> ty)
        <|> reserved "rec" $> Ast.Rec

-- ```
-- def my_fn[arg1, arg2, ...] do
--   ...stmts...
-- end
-- ```
-- OR
-- ```
-- def my_fn[arg1, arg2, ...] = ...expr...
-- ```
defExpr :: Parser Ast.Expr
defExpr = spanned do
  reserved "def"
  name <- identifier
  let param = (,) <$> identifier <*> (colon *> ty)
  params <- brackets $ sepEndBy param (symbol ",")
  let exprTail =
        ( do
            reservedOp "="
            body <- expression
            return $ Ast.DefF name params Nothing body
        )
  let blockTail =
        ( do
            retTy <- option Ty.VoidTy (reservedOp "->" *> ty) -- Without a ret ty, def defaults to returning Unit.
            body <- blockExpr
            return $ Ast.DefF name params (Just retTy) body
        )
  exprTail <|> blockTail

ty :: Parser Ty.Ty
ty =
  try (Ty.TupleTy <$> (symbol "Tuple" *> brackets (sepEndBy ty comma)))
    <|> (Ty.AliasTy <$> (reserved "rec" *> identifier))
    <|> (Ty.AliasTy <$> identifier)
    <|> (Ty.FnTy <$> brackets (sepEndBy ty comma) <*> (reservedOp "->" *> ty))
    <?> "type"

ifExpr :: Parser Ast.Expr
ifExpr = spanned do
  reserved "if"
  cond <- expression
  reserved "then"
  yesBody <- seqTerminatedBy (reserved "else")
  reserved "else"
  noBody <- seqTerminatedBy (reserved "end")
  reserved "end"
  return $ Ast.IfF cond yesBody noBody

matchExpr :: Parser Ast.Expr
matchExpr = spanned do
  reserved "match"
  scrutinee <- expression
  arms <- many $ (,) <$> (reservedOp "|" *> refutPat <* reservedOp "=>") <*> seqTerminatedBy (reservedOp "|" <|> reserved "end")
  reserved "end"
  return $ Ast.MatchF scrutinee arms

whileExpr :: Parser Ast.Expr
whileExpr = spanned do
  reserved "while"
  cond <- expression
  body <- expression
  return $ Ast.WhileF cond body

loopExpr :: Parser Ast.Expr
loopExpr = spanned do
  reserved "loop"
  body <- expression
  return $ Ast.LoopF body

nopExpr :: Parser Ast.Expr
nopExpr = spanned $ reserved "nop" *> return Ast.NopF

letExpr :: Parser Ast.Expr
letExpr = spanned $ Ast.LetF <$> (reserved "let" *> pat) <*> (reservedOp "=" *> expression)

letElseExpr :: Parser Ast.Expr
letElseExpr = spanned $ do
  reserved "let"
  pat <- refutPat
  reservedOp "="
  expr <- expression
  reserved "else"
  alt <- seqTerminatedBy $ reserved "end"
  reserved "end"
  return $ Ast.LetElseF pat expr alt

assignExpr :: Parser Ast.Expr
assignExpr = spanned $ Ast.AssignF <$> identifier <*> (reservedOp "=" *> expression)

letConstExpr :: Parser Ast.Expr
letConstExpr = spanned $ Ast.LetConstF <$> (reserved "let" *> reserved "const" *> identifier) <*> (reservedOp "=" *> expression)

returnExpr :: Parser Ast.Expr
returnExpr = spanned $ Ast.RetF <$> (reserved "ret" *> expression)

-- Parse a pattern.
pat :: Parser Ast.Pat
pat =
  (Ast.VarPat <$> identifier)
    <|> (Ast.TuplePat <$> braces (sepEndBy pat comma))
    <?> "irrefutable pattern"

refutPat :: Parser Ast.RefutPat
refutPat =
  (Ast.VarRefutPat <$> identifier)
    <|> try vrntRefutPat
    <|> (Ast.TupleRefutPat <$> braces (sepEndBy refutPat comma))
    <?> "refutable pattern"

vrntRefutPat :: Parser Ast.RefutPat
vrntRefutPat = braces do
  colon
  name <- identifier
  params <- sepEndBy refutPat comma
  return $ Ast.VrntRefutPat (':' : name) params

unaryOp op f = do
  start <- getPosition
  op
  end <- getPosition
  return $ \x -> f x :@: mkSpan start end

binaryOp op f = do
  start <- getPosition
  op
  end <- getPosition
  return $ \x y -> f x y :@: mkSpan start end

operators :: [[Operator String () Data.Functor.Identity.Identity Ast.Expr]]
operators =
  [ [ Prefix (unaryOp (reservedOp "-") (Ast.UnaryF Ast.Neg)),
      Prefix (unaryOp (reserved "not") (Ast.UnaryF Ast.Not))
    ],
    [ Infix (binaryOp (reservedOp "*") (Ast.BinaryF (Ast.ArithOp Ast.Mul))) AssocLeft,
      Infix (binaryOp (reservedOp "/") (Ast.BinaryF (Ast.ArithOp Ast.Div))) AssocLeft
    ],
    [ Infix (binaryOp (reservedOp "+") (Ast.BinaryF (Ast.ArithOp Ast.Add))) AssocLeft,
      Infix (binaryOp (reservedOp "-") (Ast.BinaryF (Ast.ArithOp Ast.Sub))) AssocLeft,
      Infix (binaryOp (reservedOp "++") (Ast.BinaryF (Ast.OtherOp Ast.Concat))) AssocLeft
    ],
    [ Infix (binaryOp (reservedOp ">") (Ast.BinaryF (Ast.RelOp Ast.Gt))) AssocLeft,
      Infix (binaryOp (reservedOp "<") (Ast.BinaryF (Ast.RelOp Ast.Lt))) AssocLeft
      -- , Infix (binaryOp (reservedOp "==") (Ast.BinaryF (Ast.RelOp Ast.Eq)))  AssocLeft
      -- , Infix (binaryOp (reservedOp "!=") (Ast.BinaryF (Ast.RelOp Ast.Neq))) AssocLeft
    ],
    [ Infix (binaryOp (reserved "and") (Ast.BinaryF (Ast.BoolOp Ast.And))) AssocLeft,
      Infix (binaryOp (reserved "or") (Ast.BinaryF (Ast.BoolOp Ast.Or))) AssocLeft,
      Infix (binaryOp (reserved "xor") (Ast.BinaryF (Ast.BoolOp Ast.Xor))) AssocLeft
    ]
  ]

-- NOTE: Function application syntax is left-recursive!
-- That's why we gotta split things up into `termFirst`
-- and `term`.
term :: Parser Ast.Expr
term =
  try nestedCalls
    <|> try nestedAnn
    <|> try nestedProj
    <|> termFirst
    <?> "term"

-- my_func[x, y][1, 2][a, b, c]
nestedCalls :: Parser Ast.Expr
nestedCalls = do
  start <- getPosition
  head <- termFirst
  allArgs <- many1 ((,) <$> arguments <*> getPosition)
  return $ foldl (reducer start) head allArgs -- Build up all the calls
  where
    reducer start expr (arg, end) = Ast.CallF expr arg :@: mkSpan start end

-- 1 : Never : Nat : Int
nestedAnn :: Parser Ast.Expr
nestedAnn = do
  start <- getPosition
  head <- termFirst
  allTys <- many1 ((,) <$> (reserved "as" *> ty) <*> getPosition)
  return $ foldl (reducer start) head allTys -- Build up all the annotations
  where
    reducer start expr (ty, end) = Ast.AnnF expr ty :@: mkSpan start end

-- x.1.4.0.2
nestedProj :: Parser Ast.Expr
nestedProj = do
  start <- getPosition
  head <- termFirst
  allProjs <- many1 ((,) <$> (dot *> natural) <*> getPosition)
  return $ foldl (reducer start) head allProjs -- Build up all the projections
  where
    reducer start expr (idx, end) = Ast.UnaryF (Ast.TupleProj idx) expr :@: mkSpan start end

varExpr :: Parser Ast.Expr
varExpr = spanned $ Ast.VarF <$> identifier

literalExpr :: Parser Ast.Expr
literalExpr = spanned $ Ast.LiteralF <$> literal

termFirst :: Parser Ast.Expr
termFirst = semiEndedTerm <|> endEndedTerm

semiEndedTerm =
  intrinsicExpr
    <|> parens expression
    <|> nopExpr
    <|> returnExpr
    <|> try letConstExpr
    <|> letExpr
    <|> literalExpr
    <|> try assignExpr
    <|> varExpr
    <?> "term that ends with `;`"

endEndedTerm =
  blockExpr
    <|> ifExpr
    <|> matchExpr
    <|> whileExpr
    <|> loopExpr
    <|> letElseExpr
    <|> defExpr
    <|> modExpr
    <|> tyDefExpr
    <?> "term that ends with `end`"

endEndedExpr = endEndedTerm

-- Expressions
expression :: Parser Ast.Expr
expression = buildExpressionParser operators term

terminatedExpr :: Parser Ast.Expr
terminatedExpr = try (expression <* semi) <|> endEndedExpr

intrinsicExpr :: Parser Ast.Expr
intrinsicExpr = spanned do
  pos <- getPosition
  reserved "intr"
  dot
  name <- identifier
  args <- arguments
  return $ Ast.IntrinsicF pos name args

callExpr :: Parser Ast.Expr
callExpr = spanned $ Ast.CallF <$> expression <*> arguments

arguments :: Parser [Ast.Expr]
arguments = brackets $ sepEndBy expression comma

literal :: Parser (Ast.Lit Ast.Expr)
literal =
  (Ast.Int <$> integer)
    <|> (Ast.Bool <$> boolean)
    <|> (Ast.Text <$> stringLiteral)
    <|> try (Ast.Tuple <$> tuple)
    <|> vrnt
    <?> "literal"
  where
    boolean =
      (reserved "true" >> return True)
        <|> (reserved "false" >> return False)
    tuple = braces $ sepEndBy expression comma
    vrnt = braces do
      colon
      name <- identifier
      args <- sepEndBy expression comma
      return $ Ast.Vrnt (':' : name) args

seqTerminatedBy :: Parser a -> Parser (Ast.Seq Ast.Expr)
seqTerminatedBy end =
  -- First case: terminatedExpr IMMEDIATELY FOLLOWED BY end, should be put in
  -- `Ast.Result`, NOT `Ast.Semi _ Ast.End`. This allows `if` exprs to appear
  -- at the end of a block and return their result without looking like they
  -- return `Void`.
  try (Ast.Result <$> (terminatedExpr <* lookAhead end))
    <|> try (Ast.Semi <$> terminatedExpr <*> seqTerminatedBy end)
    <|> (Ast.Result <$> (expression <* lookAhead end))
    <|> (lookAhead end $> Ast.Empty)
    <?> "sequence of expressions"

blockExpr :: Parser Ast.Expr
blockExpr = spanned do
  reserved "do"
  seq <- seqTerminatedBy (reserved "end")
  reserved "end"
  return $ Ast.BlockF seq

-- REPL Helper Functions
parseString :: String -> Ast.Expr
parseString = parseSrc "<string input>"

parseSrc :: String -> String -> Ast.Expr
parseSrc fileName src = do
  case parse langParser fileName src of
    Left e -> error $ "Parse Error:" +++ code e
    Right (Ast.ModF _ items :@: loc) ->
      let modName = modNameFromFileName fileName
       in Ast.ModF modName items :@: loc
    Right _ -> undefined

parseFile :: String -> IO Ast.Expr
parseFile fileName = do
  src <- readFile fileName
  return $ parseSrc fileName src

modNameFromFileName :: String -> String
modNameFromFileName fileName = camelCase $ splitName $ takeBaseName fileName

splitName :: String -> [String]
splitName = go ""
  where
    go acc = \case
      "" -> [acc]
      c : cs
        | c `elem` ['-', '_'] -> acc : go "" cs
        | otherwise -> go (acc ++ [c]) cs

camelCase :: [String] -> String
camelCase = concatMap capitalize
  where
    capitalize (c : cs) = Char.toUpper c : cs
    capitalize [] = undefined

spanned :: Parser (Ast.ExprF (At Ast.ExprF)) -> Parser (At Ast.ExprF)
spanned p = do
  p1 <- getPosition
  exprF <- p
  p2 <- getPosition
  let pos = mkSpan p1 p2
  return $ exprF :@: pos
