{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Data.Functor.Identity
import Data.Text
import Data.Void
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Text.Megaparsec

import Parser
import AbstractSyntaxTree

parseStr :: Parser a -> Text -> Either (ParseError Char Void) a
parseStr parser = runIdentity . (runParserT parser "")
validParse = Right

unitTests = testGroup "Parser Unit Tests"
  [ testCase "varId parses a variable id" varIdHappyPath
  , testCase "typeId parses a type id" typeIdHappyPath
  , testCase "lvalue parses a single id" lvalueSingleId
  , testCase "lvalue parses member access" lvalueMemberAccess
  , testCase "lvalue parses subscript access" lvalueSubscriptAccess
  , testCase "dec parses var without type" decVarDecWithoutType
  , testCase "expr parses array creation" exprArrayExpr
  ]

varIdHappyPath = parseStr varId "a_valid_identifier_with09" @?= validParse (Id "a_valid_identifier_with09")

typeIdHappyPath = parseStr typeId "a_val1d_typ3_id" @?= validParse (TypeId "a_val1d_typ3_id")

lvalueSingleId = parseStr lvalue "var_name" @?= validParse (var (Id "var_name"))

lvalueMemberAccess = let
  actual = parseStr lvalue "var_name.member_name"
  expected = validParse $ member ((var . Id) "var_name") (Id "member_name")
  in actual @?= expected

lvalueSubscriptAccess = let
  actual = parseStr lvalue "array_name[10]"
  expected = validParse $ subscript (var $ Id "array_name") (intExpr 10)
  in actual @?= expected

decVarDecWithoutType = let
  actual = parseStr dec "var arr1 := arrtype[10] of 0"
  expected = validParse $ VarDec (Id "arr1") Nothing $ arrayExpr (TypeId "arrtype") (intExpr 10) (intExpr 0)
  in actual @?= expected

exprArrayExpr = let
  actual = parseStr expr "arrtype[10] of 0"
  expected = validParse $ arrayExpr (TypeId "arrtype") (intExpr 10) (intExpr 0)
  in actual @?= expected

whileMutlipleConditions = let
  bufLval = (lvalueExpr . var) (Id "buffer")
  actual = parseStr expr "while buffer=\" \" | buffer=\"\\n\" do buffer := getchar()"
  expected = validParse $ whileExpr (infixExpr Or (infixExpr Eq bufLval (stringExpr " ")) (infixExpr Eq bufLval "\\n")) (assignExpr bufLval (callExpr (Id "getchar") []))
  in actual @?= expected
