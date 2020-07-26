{-# LANGUAGE NamedFieldPuns #-}
module Pretty where

import Data.List (intersperse)

import Syntax

------------------------------------------------------------------------------------------
-- Print Terms
------------------------------------------------------------------------------------------

printTerm :: Term -> String
printTerm (TmLit i) = show i
printTerm (TmVar v) = v
printTerm (TmLam x t) = "\\" <> x <> "." <> printTerm t
printTerm (TmApp t1 t2) = "(" <> printTerm t1 <> " " <> printTerm t2 <> ")"
printTerm (TmRcd fs) = "{" <> concat (printFoo <$> fs) <> "}"
  where
    printFoo (lbl, tm) = lbl <> " = " <> printTerm tm <> ","
printTerm (TmSel tm lbl) = printTerm tm <> "." <> lbl

------------------------------------------------------------------------------------------
-- Print simple types
------------------------------------------------------------------------------------------

printSimpleType :: SimpleType -> String
printSimpleType (TyVar v) = v
printSimpleType (TyPrim n) = n
printSimpleType (TyFun t1 t2) =
  let
    t1p = printSimpleType t1
    t2p = printSimpleType t2
  in
    "(" <> t1p <> " -> " <> t2p <> ")"
printSimpleType (TyRcd fs) =
  let
    foo (lbl, ty) = lbl <> " : " <> printSimpleType ty <> ", "
  in
    "{" <> concat (map foo fs) <> "}"

printVariableState :: VariableState -> String
printVariableState MkVariableState { lowerBounds, upperBounds } =
  let
    lbp = map printSimpleType lowerBounds
    ubp = map printSimpleType upperBounds
  in
    "< lower: " <> concat (intersperse "," lbp) <> " upper: " <> concat (intersperse "," ubp) <> " >"
