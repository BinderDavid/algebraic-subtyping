{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module SolveConstraints where

import Data.Map (Map)
import qualified Data.Map as M

import Syntax
import GenerateConstraints

------------------------------------------------------------------------------------------
-- Constraint Solving
------------------------------------------------------------------------------------------

solveConstraint :: Constraint -> (Map UVar VariableState -> Map UVar VariableState)
solveConstraint (SubType (TyPrim n) (TyPrim n')) | n == n' = id
solveConstraint (SubType (TyFun l0 r0) (TyFun l1 r1)) = solveConstraint (SubType l1 l0) . solveConstraint (SubType r0 r1)
solveConstraint (SubType (TyRcd fs0) (TyRcd fs1)) = \m ->
  let
    foo (lbl, ty) = case lookup lbl fs0 of
      Nothing -> \_ -> error "Record field does not exist"
      Just ty' -> solveConstraint (SubType ty' ty)
    foos = foo <$> fs1
  in
    foldr (.) id foos m
solveConstraint (SubType (TyVar v) ty) = \m ->
  case M.lookup v m of
    Nothing -> M.insert v (MkVariableState [] [ty]) m
    Just (MkVariableState { lowerBounds, upperBounds }) ->
      let
        newConstraints = (\lb -> SubType lb ty) <$> lowerBounds
        updatedMap = M.insert v (MkVariableState lowerBounds (ty : upperBounds)) m
      in
        foldr (.) id (solveConstraint <$> newConstraints) $ updatedMap
solveConstraint (SubType ty (TyVar v)) = \m ->
  case M.lookup v m of
    Nothing -> M.insert v (MkVariableState [ty] []) m
    Just (MkVariableState { lowerBounds, upperBounds }) ->
      let
        newConstraints = (\ub -> SubType ty ub) <$> upperBounds
        updatedMap = M.insert v (MkVariableState (ty : lowerBounds) upperBounds) m
      in
        foldr (.) id (solveConstraint <$> newConstraints) $ updatedMap
solveConstraint (SubType _ty1 _ty2) = error "Cannot constrain types"


data ConstraintSolverState = ConstraintSolverState
  { css_constraints :: [Constraint]
  , css_partialResult :: Map UVar VariableState
  , css_cache :: [Constraint]
  }

stepConstraintSolver :: ConstraintSolverState -> Maybe ConstraintSolverState
stepConstraintSolver ConstraintSolverState { css_constraints = [] } = Nothing
stepConstraintSolver ConstraintSolverState { css_constraints = constraint : constraints, css_partialResult, css_cache } =
  if constraint `elem` css_cache
  then Just ConstraintSolverState { css_constraints = constraints, css_partialResult = css_partialResult, css_cache = css_cache }
  else Just ConstraintSolverState { css_constraints = constraints
                                  , css_partialResult = solveConstraint constraint css_partialResult
                                  , css_cache = constraint : css_cache
                                  }

stepUntilFinished :: [Constraint] -> [UVar] -> [ConstraintSolverState]
stepUntilFinished constraints uvars = initialState : stepStates initialState
  where
    initialState = ConstraintSolverState { css_constraints = constraints
                                         , css_partialResult = M.fromList ((\uvar -> (uvar,MkVariableState [] [])) <$> uvars)
                                         , css_cache = [] }
    stepStates s = case stepConstraintSolver s of
      Nothing -> []
      Just s' -> s' : stepStates s'

solveConstraints :: [Constraint] -> [UVar] ->  Map UVar VariableState
solveConstraints constraints uvars = css_partialResult (last (stepUntilFinished constraints uvars))

