{-# LANGUAGE NamedFieldPuns #-}
module Pretty where

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

printSimpleType :: SimpleType -> String
printSimpleType (TyVar v) = v
printSimpleType (TyPrim p) = printPrimitive p
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

------------------------------------------------------------------------------------------
-- Print resolved types
------------------------------------------------------------------------------------------

printSimpleTypeR :: SimpleTypeR -> String
printSimpleTypeR (TyVarR vs) = printVariableStateR vs
printSimpleTypeR (TyPrimR p) = printPrimitive p
printSimpleTypeR (TyFunR t1 t2) =
  let
    t1p = printSimpleTypeR t1
    t2p = printSimpleTypeR t2
  in
    "(" <> t1p <> " -> " <> t2p <> ")"
printSimpleTypeR (TyRcdR fs) =
  let
    foo (lbl, ty) = lbl <> " : " <> printSimpleTypeR ty <> ", "
  in
    "{" <> concat (map foo fs) <> "}"

printVariableStateR :: VariableStateR -> String
printVariableStateR MkVariableStateR { lowerBoundsR, upperBoundsR } =
  let
    lbp = map printSimpleTypeR lowerBoundsR
    ubp = map printSimpleTypeR upperBoundsR
  in
    "< lower: " <> concat (intersperse "," lbp) <> " upper: " <> concat (intersperse "," ubp) <> " >"

------------------------------------------------------------------------------------------
-- Print resolved types
------------------------------------------------------------------------------------------

printConstraint :: Constraint -> String
printConstraint (SubType ty1 ty2) = printSimpleType ty1 <> " <: " <> printSimpleType ty2

printCSS :: ConstraintSolverState -> String
printCSS ConstraintSolverState { css_constraints, css_partialResult, css_cache } =
  unlines [ "---------------------------------------------------------------------------------------"
          , "Constraints:"
          , constraints
          , "Partial result:"
          , partialResult
          , "Cache:"
          , cache
          , "---------------------------------------------------------------------------------------"
          ]
  where
    constraints = concat (intersperse "\n" (printConstraint <$> css_constraints))
    printPartialResult (var, vs) = var <> " => " <> printVariableState vs
    partialResult = concat (intersperse "\n" (printPartialResult <$> (M.assocs css_partialResult)))
    cache = concat (intersperse "\n" (printConstraint <$> css_cache))




inferIO :: Term -> IO SimpleTypeR
inferIO tm = do
  putStrLn ("Inferring type for term: " <> printTerm tm)
  let (typ, constraints) = runGenerateM (typeTerm tm)
  putStrLn ("Inferred the type: " <> printSimpleType typ)
  putStrLn "Start constraint solving..."
  let solverStates = stepUntilFinished constraints
  let ppSolverStates = unlines (printCSS <$> solverStates)
  putStrLn ppSolverStates
  putStrLn "Start zonking..."
  let finalResult = css_partialResult (last (stepUntilFinished constraints))
  let res = zonk typ finalResult
  putStrLn "Final result:"
  putStrLn (printSimpleTypeR res)
  return res

