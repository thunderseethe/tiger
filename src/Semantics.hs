{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Semantics where

import           Data.Functor.Compose
import           Data.Functor.Foldable
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe

import           AbstractSyntaxTree

type Env id = HashMap id Binding

newtype Binding = Binding { typ :: Maybe Type }

typed :: Type -> Binding
typed = Binding . Just

untyped :: Binding
untyped = Binding Nothing

data Scope = Scope { typeEnv :: Env TypeId, valEnv :: Env Id, enclosingScope :: Maybe Scope }

emptyScope :: Scope
emptyScope = Scope HM.empty HM.empty Nothing

mapTypeEnv :: Scope -> (Env TypeId -> Env TypeId) -> Scope
mapTypeEnv scope f = scope { typeEnv = f (typeEnv scope)}

mapValEnv :: Scope -> (Env Id -> Env Id) -> Scope
mapValEnv scope f = scope { valEnv = f (valEnv scope)}

enclose :: Maybe Scope -> Scope -> Scope
enclose outter inner = inner { enclosingScope = outter}

type ScopedExpr = Fix (Compose ((,) Scope) ExprF)
fixScope :: (Scope, ExprF ScopedExpr) -> ScopedExpr
fixScope = Fix . Compose

buildScopeContinuation ::
     Base Expr (Maybe Scope -> ScopedExpr) -> Maybe Scope -> ScopedExpr
buildScopeContinuation expr s = let
  defaultScope = fromMaybe emptyScope s
  in fixScope $ case expr of
    SeqExpr scopedExprs ->
      (defaultScope, SeqExpr $ ($ s) <$> scopedExprs)
    CallExpr {funcName, params} ->
        ( defaultScope
        , CallExpr funcName $ ($ s) <$> params)
    IfThenExpr {cond, thenExpr, elseExpr} -> (defaultScope, IfThenExpr (cond s) (thenExpr s) (($ s) <$> elseExpr))
    WhileExpr {cond, body} -> (defaultScope, WhileExpr (cond s) (body s))
