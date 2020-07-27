{-# LANGUAGE NamedFieldPuns #-}
module Pretty where

import Control.Monad (forM_)
import Data.List (intersperse)
import qualified Data.Map as M

import Syntax
import Inference

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

printPrimitive :: Primitive -> String
printPrimitive PrimInt = "Int"

printUVar :: UVar -> String
printUVar MkUVar { uvar_name } = "U" <> show uvar_name

printSimpleType :: SimpleType -> String
printSimpleType (TyVar v) = printUVar v
printSimpleType (TyPrim p) = printPrimitive p
printSimpleType (TyFun ty1 ty2) = "(" <> printSimpleType ty1 <> " -> " <> printSimpleType ty2 <> ")"
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

------------------------------------------------------------------------------------------
-- Print Target types
------------------------------------------------------------------------------------------

printTargetType :: TargetType -> String
printTargetType (TTyPrim p) = printPrimitive p
printTargetType TTyTop = "Top"
printTargetType TTyBot = "Bot"
printTargetType (TTyUnion ty1 ty2) = "(" <> printTargetType ty1 <> " \\/ " <> printTargetType ty2 <> ")"
printTargetType (TTyInter ty1 ty2) = "(" <> printTargetType ty1 <> " /\\ " <> printTargetType ty2 <> ")"
printTargetType (TTyFun ty1 ty2) = "(" <> printTargetType ty1 <> " -> " <> printTargetType ty2 <> ")"
printTargetType (TTyVar (Pos, v)) = "+" <> printUVar v
printTargetType (TTyVar (Neg, v)) = "-" <> printUVar v
printTargetType (TTyRcd fs) =
  let
    foo (lbl, ty) = lbl <> " : " <> printTargetType ty <> ", "
  in
    "{" <> concat (map foo fs) <> "}"
printTargetType (TTyRec v ty) = "mu " <> v <> "." <> printTargetType ty

------------------------------------------------------------------------------------------
-- Print Constraint Solver States
------------------------------------------------------------------------------------------

printConstraint :: Constraint -> String
printConstraint (SubType ty1 ty2) = printSimpleType ty1 <> " <: " <> printSimpleType ty2

printCSS :: ConstraintSolverState -> String
printCSS ConstraintSolverState { css_constraints, css_partialResult, css_cache } =
  unlines [ "==="
          , "Constraints:"
          , ppConstraints css_constraints
          , "Partial result:"
          , partialResult
          , "Cache:"
          , ppConstraints css_cache
          ]
  where
    ppConstraints constraints = concat (intersperse "\n" ((\constraint -> "     " <> printConstraint constraint) <$> constraints))
    printPartialResult (var, vs) = "     " <> printUVar var <> " => " <> printVariableState vs
    partialResult = concat (intersperse "\n" (printPartialResult <$> (M.assocs css_partialResult)))

