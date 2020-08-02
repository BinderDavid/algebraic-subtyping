{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SolveConstraints where

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor (bimap)

import Syntax
import GenerateConstraints

------------------------------------------------------------------------------------------
-- The Constraint Solver Monad
------------------------------------------------------------------------------------------

type Error = String

data ConstraintSolverState = ConstraintSolverState
  { css_constraints :: [Constraint]
  , css_partialResult :: Map UVar VariableState
  , css_cache :: [Constraint]
  }

newtype SolveM a = MkSolveM
  { unSolveM :: StateT ConstraintSolverState (Except Error) a
  } deriving (Functor, Applicative, Monad, MonadState ConstraintSolverState, MonadError Error)

evalSolveM :: ConstraintSolverState -> SolveM a -> Either Error ConstraintSolverState
evalSolveM css solver = runExcept (execStateT (unSolveM solver) css)

------------------------------------------------------------------------------------------
-- Primitive Operations in the constraint solver Monad
------------------------------------------------------------------------------------------

lookupUVar :: UVar -> SolveM VariableState
lookupUVar uv = do
  ConstraintSolverState { css_partialResult } <- get
  case M.lookup uv css_partialResult of
    Nothing -> do
      let foo css@ConstraintSolverState { css_partialResult } = css { css_partialResult = M.insert uv (MkVariableState [] []) css_partialResult }
      modify foo
      return (MkVariableState [] [])
    Just vs -> return vs

-- | Add a lower bound ty to the unification variable uv.
addLowerBound :: UVar -> SimpleType -> SolveM ()
addLowerBound uv ty = modify alb
  where
    alb css@ConstraintSolverState { css_partialResult } = css { css_partialResult = alb' css_partialResult }
    alb' m = M.adjust (\(MkVariableState lbs ubs) -> MkVariableState (ty:lbs) ubs) uv m

-- | Add an upper bound ty to the unification variable uv.
addUpperBound :: UVar -> SimpleType -> SolveM ()
addUpperBound uv ty = modify aub
  where
    aub css@ConstraintSolverState { css_partialResult } = css { css_partialResult = aub' css_partialResult }
    aub' m = M.adjust (\(MkVariableState lbs ubs) -> MkVariableState lbs (ty:ubs)) uv m

-- | Add the constraint c to the cache.
addConstraintToCache :: Constraint -> SolveM ()
addConstraintToCache c = modify addc
  where
    addc css@ConstraintSolverState { css_cache } = css { css_cache = c:css_cache }

-- | Check whether the constraint c is in the cache.
checkCache :: Constraint -> SolveM Bool
checkCache c = do
  ConstraintSolverState { css_cache } <- get
  return (c `elem` css_cache)

-- | Push a constraint on the stack.
pushConstraint :: Constraint -> SolveM ()
pushConstraint c = modify addc
  where
    addc css@ConstraintSolverState { css_constraints } = css { css_constraints = c : css_constraints }

-- | Pop a constraint from the stack.
popConstraint :: SolveM (Maybe Constraint)
popConstraint = do
  ConstraintSolverState { css_constraints } <- get
  case css_constraints of
    [] -> return Nothing
    (c:cs) -> do
      modify (\css -> css { css_constraints = cs })
      return (Just c)

snapshot :: SolveM ()
snapshot = undefined

------------------------------------------------------------------------------------------
-- Handling a single constraint:
------------------------------------------------------------------------------------------

solveConstraint :: Constraint -> SolveM ()
solveConstraint (SubType (TyPrim n) (TyPrim n')) | n == n' = return ()
solveConstraint (SubType (TyFun l0 r0) (TyFun l1 r1)) = do
  pushConstraint (SubType l1 l0)
  pushConstraint (SubType r0 r1)
solveConstraint (SubType (TyRcd fs0) (TyRcd fs1)) = do
  forM_ fs1 $ \(lbl,ty) -> do
    case lookup lbl fs0 of
      Nothing -> throwError ("Record field " <> lbl <> " does not exist")
      Just ty' -> pushConstraint (SubType ty' ty)
solveConstraint (SubType (TyVar uv) ty) = do
  addUpperBound uv ty
  MkVariableState { lowerBounds } <- lookupUVar uv
  forM_ lowerBounds $ \lb -> pushConstraint (SubType lb ty)
solveConstraint (SubType ty (TyVar uv)) = do
  addLowerBound uv ty
  MkVariableState { upperBounds } <- lookupUVar uv
  forM_ upperBounds $ \ub -> pushConstraint (SubType ty ub)
solveConstraint (SubType ty1 ty2) = do
  throwError ("Could not solve the constraint between " <> show ty1 <> " and " <> show ty2)

------------------------------------------------------------------------------------------
-- Running the constraint solver
------------------------------------------------------------------------------------------

-- | Run until there are no more constraints.
run :: SolveM ()
run = do
  maybeConstraint <- popConstraint
  case maybeConstraint of
    Nothing -> return ()
    Just constraint -> do
      cacheHit <- checkCache constraint
      case cacheHit of
        True -> run
        False -> addConstraintToCache constraint >> solveConstraint constraint >> run


solveConstraints :: [Constraint] ->  Either Error ([ConstraintSolverState],Map UVar VariableState)
solveConstraints constraints =
  let
    initialState = ConstraintSolverState
      { css_constraints = constraints
      , css_partialResult = M.empty
      , css_cache = []
      }
    finalState = evalSolveM initialState run
  in
    bimap id (\fs -> ([fs], css_partialResult fs)) finalState
