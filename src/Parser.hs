{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Parser where

import           Control.Applicative        hiding (many, some)
import           Control.Monad.Identity     (Identity, runIdentity)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import           AbstractSyntaxTree

type ParserT e s = ParsecT e s Identity

type Parser = ParserT Void Text

parseProgram :: Text -> Either (ParseError Char Void) Program
parseProgram = runIdentity . runParserT programP ""

programP :: Parser Program
programP = Program <$> (spaceConsumer >> expr)

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

assignEq :: Parser Text
assignEq = symbol ":="

reserved :: Text -> Parser ()
reserved word = (lexeme . try) (string word *> notFollowedBy alphaNumChar)

idParser :: Parser Text
idParser = (lexeme . try) (p >>= reservedCheck)
  where
    p = Text.cons <$> letterChar <*> idTail
    idTail =
      Text.pack <$>
      many (oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_'])
    reservedCheck x =
      if x `elem` reserveds
        then fail $ "keyword " ++ show x ++ " cannot be used as identifier"
        else return x

varId :: Parser Id
varId = Id <$> idParser

typeId :: Parser TypeId
typeId = TypeId <$> idParser

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

stringLiteral :: Parser Text
stringLiteral = lexeme str
  where
    str = Text.pack <$> stringP
    stringP = char '"' >> manyTill L.charLiteral (char '"')

reserveds :: [Text]
reserveds =
  [ "array"
  , "if"
  , "then"
  , "else"
  , "while"
  , "for"
  , "to"
  , "do"
  , "let"
  , "in"
  , "end"
  , "of"
  , "break"
  , "nil"
  , "function"
  , "var"
  , "type"
  ]

maybeCompose :: Maybe (a -> a) -> (a -> a) -> (a -> a)
maybeCompose maybeF g =
  case maybeF of
    Just f  -> f . g
    Nothing -> g

lvalue :: Parser (LValue Expr)
lvalue = do
  lval <- varP
  maybeCont <- lvalue'
  return (maybeCompose maybeCont id lval)
  where
    varP = var <$> varId

lvalue' :: Parser (Maybe ((LValue Expr) -> (LValue Expr)))
lvalue' = optional (subscriptP <|> memberP)

subscriptP :: Parser ((LValue Expr) -> (LValue Expr))
subscriptP = label "subscript" $ do
  indexExpr <- brackets expr
  maybeCont <- lvalue'
  return (maybeCompose maybeCont (`subscript` indexExpr))

memberP :: Parser ((LValue Expr) -> (LValue Expr))
memberP = label "member" $ do
  _ <- symbol "."
  memberId <- varId
  maybeCont <- lvalue'
  return (maybeCompose maybeCont (`member` memberId))

dec :: Parser (Dec Expr)
dec = functionDec <|> varDec <|> typeDec

varDec :: Parser (Dec Expr)
varDec = VarDec
  <$> (reserved "var" *> varId)
  <*> (optional . try $ operator ":" *> typeId)
  <*> (assignEq *> expr)
  <?> "variable declaration"

functionDec :: Parser (Dec Expr)
functionDec = FunctionDec
  <$> (reserved "function" *> varId)
  <*> parens (fieldDec `sepBy` symbol ",")
  <*> (optional .try $ operator ":" *> typeId)
  <*> (symbol "=" *> expr)
  <?> "function definition"

typeDec :: Parser (Dec Expr)
typeDec = TypeDec
  <$> (reserved "type" *> typeId)
  <*> (symbol "=" *> typeValue)
  <?> "type declaration"

typeValue :: Parser Type
typeValue = baseType <|> arrayType <|> recordType
  where
    baseType = BaseType <$> typeId <?> "base type"

arrayType :: Parser Type
arrayType = ArrayType
  <$> (reserved "array" *> reserved "of" *> typeId)
  <?> "array type"

recordType :: Parser Type
recordType = RecordType
  <$> braces (fieldDec `sepBy1` symbol ",")
  <?> "record type"

fieldDec :: Parser FieldDec
fieldDec = FieldDec
  <$> varId
  <*> (symbol ":" *> typeId)
  <?> "field declaration"

operator :: Text -> Parser Text
operator n = (lexeme . try) (string n <* notFollowedBy punctuationChar)

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

term :: Parser Expr
term = seqExprP <|> nilExprP <|> intExprP <|> ifThenExprP <|> whileExprP <|> forExprP <|> letExprP <|> stringExprP <|> try callExprP <|> try arrayExprP <|> try recordExprP <|> try assignExprP <|> lvalueExprP

table :: [[Operator Parser Expr]]
table = [ [ prefix "-" negationExpr],
          [ binary "*" (infixExpr Mult),
            binary "/" (infixExpr Div) ],
          [ binary "+" (infixExpr Add),
            binary "-" (infixExpr Sub) ],
          [ binary "=" (infixExpr Eq),
            binary "<>" (infixExpr NotEq),
            binary "<=" (infixExpr LtEq),
            binary ">=" (infixExpr GtEq),
            binary ">" (infixExpr Lt),
            binary "<" (infixExpr Gt) ],
          [ binary "&" (infixExpr And),
            binary "|" (infixExpr Or) ]
        ]

expr :: Parser Expr
expr = makeExprParser term table

nilExprP :: Parser Expr
nilExprP = nilExpr <$ reserved "nil" <?> "nil expression"

intExprP :: Parser Expr
intExprP = intExpr <$> intLiteral <?> "integer expression"

stringExprP :: Parser Expr
stringExprP = stringExpr <$> stringLiteral <?> "string expression"

lvalueExprP :: Parser Expr
lvalueExprP = lvalueExpr <$> lvalue <?> "lvalue expression"

seqExprP :: Parser Expr
seqExprP = seqExpr <$> seqP <?> "sequence expression"
  where
    seqP = parens (expr `sepBy1` symbol ";")

callExprP :: Parser Expr
callExprP = callExpr
  <$> varId
  <*> parens (expr `sepBy` symbol ",")
  <?> "function call expression"

arrayExprP :: Parser Expr
arrayExprP = arrayExpr
  <$> typeId
  <*> brackets expr
  <*> (reserved "of" *> expr)
  <?> "array expression"

recordExprP :: Parser Expr
recordExprP = recordExpr
  <$> typeId
  <*> braces (fieldCreate `sepBy1` symbol ",")
  <?> "record expression"
  where
    fieldCreate = FieldCreate
      <$> varId
      <*> (symbol "=" *> expr)

assignExprP :: Parser Expr
assignExprP = assignExpr
  <$> lvalue
  <*> (assignEq *> expr)
  <?> "assignment expression"

ifThenExprP :: Parser Expr
ifThenExprP = ifThenExpr
  <$> (reserved "if" *> expr)
  <*> (reserved "then" *> expr)
  <*> optional (reserved "else" *> expr)
  <?> "if expression"

whileExprP :: Parser Expr
whileExprP = whileExpr
  <$> (reserved "while" *> expr)
  <*> (reserved "do" *> expr)
  <?> "while expression"

forExprP :: Parser Expr
forExprP = forExpr
  <$> (reserved "for" *> varId)
  <*> (assignEq *> expr)
  <*> (reserved "to" *> expr)
  <*> (reserved "do" *> expr)
  <?> "for expression"

letExprP :: Parser Expr
letExprP = letExpr
  <$> (reserved "let" *> some dec)
  <*> (reserved "in" *> expr <* reserved "end")
  <?> "let expression"

