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

printUVar :: UVar -> String
printUVar MkUVar { uvar_name } = "U" <> show uvar_name

printSimpleType :: SimpleType -> String
printSimpleType (TyVar v) = printUVar v
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
-- Print Constraint Solver States
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
    printPartialResult (var, vs) = printUVar var <> " => " <> printVariableState vs
    partialResult = concat (intersperse "\n" (printPartialResult <$> (M.assocs css_partialResult)))
    cache = concat (intersperse "\n" (printConstraint <$> css_cache))

inferIO :: Term -> IO ()
inferIO tm = do
  putStrLn ("Inferring type for term: " <> printTerm tm)
  let (typ, constraints) = runGenerateM (typeTerm tm)
  putStrLn ("Inferred the type: " <> printSimpleType typ)
  putStrLn "Start constraint solving..."
  let solverStates = stepUntilFinished constraints
  let ppSolverStates = unlines (printCSS <$> solverStates)
  putStrLn ppSolverStates

