module FrenchInference where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as M

import Syntax (Term(..), VarName, PrimName, Label)

------------------------------------------------------------------------------------------
-- Simple types and constraints
------------------------------------------------------------------------------------------

type TyVarName = String

data SimpleType
  = TyVar TyVarName
  | TyPrim PrimName
  | TyFun SimpleType SimpleType
  | TyRcd [(Label, SimpleType)]
  deriving (Eq, Ord)


data Constraint = SubType SimpleType SimpleType

------------------------------------------------------------------------------------------
-- Constraint generation
------------------------------------------------------------------------------------------

-- During constraint generation we need:
-- - A Reader for the local variable context
-- - A Writer for the generated constraints
-- - A State for generating fresh type variables
type GenerateM = ReaderT (Map VarName SimpleType) (WriterT [Constraint] (State Int))

-- runGenerateM :: GenerateM a -> (a, [Constraint])
-- runGenerateM m = ...

freshTyVar :: GenerateM TyVarName
freshTyVar = do
  gen <- get
  modify (+1)
  return ("V" ++ show gen)

lookupVar :: VarName -> GenerateM SimpleType
lookupVar v = do
  ctx <- ask
  return (ctx M.! v)

generateConstraint :: SimpleType -> SimpleType -> GenerateM ()
generateConstraint ty1 ty2 = tell [SubType ty1 ty2]

typeTerm :: Term -> GenerateM SimpleType
typeTerm (TmLit _) = return (TyPrim "Int")
typeTerm (TmVar v) = lookupVar v
typeTerm (TmLam var tm) = do
  tyvar <- TyVar <$> freshTyVar
  ty <- local (\ctx -> M.insert var tyvar ctx) (typeTerm tm)
  return (TyFun tyvar ty)
typeTerm (TmApp tm1 tm2) = do
  ty1 <- typeTerm tm1
  ty2 <- typeTerm tm2
  tyVarRes <- freshTyVar
  generateConstraint ty1 (TyFun ty2 (TyVar tyVarRes))
  return (TyVar tyVarRes)
typeTerm (TmRcd fs) = undefined
typeTerm (TmSel tm lbl) = undefined

------------------------------------------------------------------------------------------
-- Constraint Solving
------------------------------------------------------------------------------------------

data VariableState = MkVariableState
  { lowerBounds :: [SimpleType]
  , upperBounds :: [SimpleType]
  }
  deriving (Eq, Ord)

solveConstraints :: [Constraint] -> Map TyVarName VariableState
solveConstraints [] = M.empty
solveConstraints (SubType (TyPrim n) (TyPrim n') : constraints) | n == n' = solveConstraints constraints
solveConstraints (SubType (TyFun l0 r0) (TyFun l1 r1) : constraints) = solveConstraints (SubType l1 l0 : SubType l0 r1 : constraints)
solveConstraints (SubType (TyRcd fs0) (TyRcd fs1) : constraints) = undefined
solveConstraints (SubType _ty1 _ty2 : _) = error "Cannot constrain types"

------------------------------------------------------------------------------------------
-- Zonking
------------------------------------------------------------------------------------------

data VariableStateResolved = MkVariableStateResolved
  { lowerBoundsR :: [SimpleTypeResolved]
  , upperBoundsR :: [SimpleTypeResolved]
  }
  deriving (Eq, Ord)

data SimpleTypeResolved
  = TyVarR VariableStateResolved
  | TyPrimR PrimName
  | TyFunR SimpleTypeResolved SimpleTypeResolved
  | TyRcdR [(Label, SimpleTypeResolved)]
  deriving (Eq, Ord)


zonk :: SimpleType -> Map TyVarName VariableState -> SimpleTypeResolved
zonk (TyVar v) m =
  let
    vs = m M.! v
    vs' = zonkVS vs m
  in
    TyVarR vs'
zonk (TyPrim n) _ = TyPrimR n
zonk (TyFun t1 t2) m = TyFunR (zonk t1 m) (zonk t2 m)
zonk (TyRcd fs) m = TyRcdR ((\(lbl,ty) -> (lbl, zonk ty m)) <$> fs)

zonkVS :: VariableState -> Map TyVarName VariableState -> VariableStateResolved
zonkVS = undefined
