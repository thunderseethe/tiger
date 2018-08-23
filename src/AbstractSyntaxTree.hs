{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module AbstractSyntaxTree where

import           Data.Functor.Classes
import           Data.Functor.Classes.Generic
import           Data.Functor.Compose
import           Data.Functor.Foldable
import Data.List (intersperse)
import           Data.Text                    (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics

newtype Program = Program Expr
  deriving (Show, Eq)

instance Pretty Program where
  pretty (Program expr) = pretty expr

newtype Id =
  Id Text
  deriving (Show, Eq)

instance Pretty Id where
  pretty (Id t) = pretty t


newtype TypeId =
  TypeId Text
  deriving (Show, Eq)

instance Pretty TypeId where
  pretty (TypeId t) = pretty t

data FieldDec =
  FieldDec Id
           TypeId
  deriving (Show, Eq)

instance Pretty FieldDec where
  pretty (FieldDec var typ) = pretty var <+> ":" <+> pretty typ

data Type
  = BaseType TypeId
  | ArrayType TypeId
  | RecordType [FieldDec]
  deriving (Show, Eq)

instance Pretty Type where
  pretty (BaseType typeId) = pretty typeId
  pretty (ArrayType id) = "array" <+> "of" <+> pretty id
  pretty (RecordType fields) = braces (hcat (intersperse (pretty (", " :: Text)) (map pretty fields)))

data Dec r
  = TypeDec { typeName :: TypeId
            , typeVal  :: Type }
  | FunctionDec { funcName   :: Id
                , args       :: [FieldDec]
                , resultType :: Maybe TypeId
                , body       :: r }
  | VarDec { varName :: Id
           , varType :: Maybe TypeId
           , value   :: r }
  deriving (Show, Eq, Functor, Generic1)

instance Show1 Dec where
  liftShowsPrec = liftShowsPrecDefault

instance Eq1 Dec where
  liftEq = liftEqDefault

prettyDec =
  \case
    TypeDec {typeName, typeVal} ->
      "type" <+> pretty typeName <+> "=" <+> pretty typeVal
    FunctionDec {funcName, args, resultType, body} ->
      "function" <+>
      pretty funcName <>
      parens (hcat (intersperse (pretty (", " :: Text)) (fmap pretty args))) <>
      maybe emptyDoc ((<+>) ":" . pretty) resultType <+>
      "=" <+> body
    VarDec {varName, varType, value} ->
      "var" <+>
      pretty varName <> maybe emptyDoc ((<+>) ":" . pretty) varType <+>
      ":=" <+> value

data LValue e
  = Var Id
  | Subscript { variable  :: LValue e
              , indexExpr :: e }
  | Member { variable :: LValue e
          , memberId  :: Id }
    deriving (Functor, Generic1)

instance Show1 LValue where
  liftShowsPrec = liftShowsPrecDefault

instance Eq1 LValue where
  liftEq = liftEqDefault

prettyLValue :: LValue (Doc ann) -> Doc ann
prettyLValue = \case
  Var id -> pretty id
  Subscript {variable, indexExpr} -> prettyLValue variable <> brackets indexExpr
  Member {variable, memberId} -> prettyLValue variable <> "." <> pretty memberId

var :: Id -> LValue a
var = Var

subscript :: LValue Expr -> Expr -> LValue Expr
subscript variable indexExpr = Subscript {variable, indexExpr}

member :: LValue a -> Id -> LValue a
member variable memberId = Member {variable, memberId}

data Op
  = Mult
  | Div
  | Add
  | Sub
  | Eq
  | NotEq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | And
  | Or
  deriving (Show, Eq)

instance Pretty Op where
  pretty Mult  = pretty '*'
  pretty Add   = pretty '+'
  pretty Sub   = pretty '-'
  pretty Eq    = pretty '='
  pretty NotEq = pretty ("<>" :: Text)
  pretty Lt    = pretty '<'
  pretty Gt    = pretty '>'
  pretty GtEq  = pretty (">=" :: Text)
  pretty LtEq  = pretty ("<=" :: Text)
  pretty And   = pretty '&'
  pretty Or    = pretty '|'

type Expr = Fix ExprF
data ExprF r
  = NilExpr
  | BreakExpr
  | IntExpr Int
  | StringExpr Text
  | NegationExpr r
  | SeqExpr [r]
  | CallExpr { funcName :: Id
             , params   :: [r] }
  | IfThenExpr { cond     :: r
               , thenExpr :: r
               , elseExpr :: Maybe r }
  | WhileExpr { cond :: r
              , body :: r }
  | ForExpr { loopVar :: Id
            , start   :: r
            , end     :: r
            , body    :: r }
  | LetExpr { decs :: [Dec r]
            , body :: r }
  | ArrayExpr { typeName :: TypeId
              , sizeExpr :: r
              , initExpr :: r}
  | RecordExpr { typeName :: TypeId
               , fields   :: [FieldCreate r] }
  | AssignmentExpr { variable :: LValue r
                   , value    :: r }
  | InfixExpr { left :: r, op :: Op, right :: r }
  | LValueExpr { variable :: LValue r }
  deriving (Functor, Generic1)

instance Show1 ExprF where
  liftShowsPrec = liftShowsPrecDefault

instance Eq1 ExprF where
  liftEq = liftEqDefault

-- Pretty Print Algebra
prettyExpr :: Base Expr (Doc ann) -> Doc ann
prettyExpr =
  \case
    NilExpr -> pretty ("nil" :: Text)
    BreakExpr -> pretty ("break" :: Text)
    IntExpr i -> pretty i
    StringExpr s -> dquotes (pretty s)
    SeqExpr exprs -> (parens . vsep) (map (<> semi) exprs)
    CallExpr {funcName, params} ->
      pretty funcName <> (parens . hsep) (intersperse (comma <> space) params)
    IfThenExpr {cond, thenExpr, elseExpr} ->
      "if" <+>
      cond <+> "then" <+> thenExpr <+> maybe emptyDoc ("else" <+>) elseExpr
    WhileExpr {cond, body} -> "while" <+> cond <+> "do" <+> body
    ForExpr {loopVar, start, end, body} ->
      "for" <+>
      pretty loopVar <+> ":=" <+> start <+> "to" <+> end <+> "do" <> line <> indent 4 body
    LetExpr {decs, body} ->
      "let" <> line <> indent 4 (vsep (fmap prettyDec decs)) <>
      line <>
      "in" <+>
      line <> indent 4 body <>
      line
    ArrayExpr {typeName, sizeExpr, initExpr} ->
      pretty typeName <> brackets sizeExpr <+> "of" <+> initExpr
    RecordExpr {typeName, fields} ->
      pretty typeName <>
      braces
        (hcat
           (intersperse (pretty (", " :: Text)) (map prettyFieldCreate fields)))
    AssignmentExpr {variable, value} -> prettyLValue variable <+> ":=" <+> value
    InfixExpr {left, op, right} -> left <+> pretty op <+> right
    LValueExpr {variable} -> prettyLValue variable

instance Pretty Expr where
  pretty = cata prettyExpr

data FieldCreate r =
  FieldCreate Id
              r
  deriving (Functor, Show, Eq)

instance Show1 FieldCreate where
  liftShowsPrec shwP _ p (FieldCreate i r) = showsBinaryWith showsPrec shwP "FieldCreate" p i r

instance Eq1 FieldCreate where
  liftEq eq (FieldCreate idA a) (FieldCreate idB b) = idA == idB && eq a b


prettyFieldCreate (FieldCreate id expr) = pretty id <+> "=" <+> expr

nilExpr :: Expr
nilExpr = Fix NilExpr

intExpr :: Int -> Expr
intExpr = Fix . IntExpr

stringExpr :: Text -> Expr
stringExpr = Fix . StringExpr

negationExpr :: Expr -> Expr
negationExpr = Fix . NegationExpr

seqExpr :: [Expr] -> Expr
seqExpr = Fix . SeqExpr

callExpr :: Id -> [Expr] -> Expr
callExpr funcName params = Fix CallExpr {funcName, params}

ifThenExpr :: Expr -> Expr -> Maybe Expr -> Expr
ifThenExpr cond thenExpr elseExpr = Fix IfThenExpr {cond, thenExpr, elseExpr}

arrayExpr :: TypeId -> Expr -> Expr -> Expr
arrayExpr typeName sizeExpr initExpr =
  Fix ArrayExpr {typeName, sizeExpr, initExpr}

recordExpr :: TypeId -> [FieldCreate Expr] -> Expr
recordExpr typeName fields = Fix RecordExpr {typeName, fields}

assignExpr :: LValue Expr -> Expr -> Expr
assignExpr variable value = Fix AssignmentExpr {variable, value}

whileExpr :: Expr -> Expr -> Expr
whileExpr cond body = Fix WhileExpr {cond, body}

forExpr :: Id -> Expr -> Expr -> Expr -> Expr
forExpr loopVar start end body = Fix ForExpr {loopVar, start, end, body}

letExpr :: [Dec Expr] -> Expr -> Expr
letExpr decs body = Fix LetExpr {decs, body}

infixExpr :: Op -> Expr -> Expr -> Expr
infixExpr op left right = Fix InfixExpr {left, op, right}

lvalueExpr :: LValue Expr -> Expr
lvalueExpr = Fix . LValueExpr
