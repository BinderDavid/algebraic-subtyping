module Syntax where

import Control.Monad (forM)
import Data.IORef

type PrimName = String
type VarName = String
type Label = String

------------------------------------------------------------------------------------------
-- Terms
------------------------------------------------------------------------------------------

data IsRec = Rec | NonRec

data Term
  = TmLit Int
  | TmVar VarName
  | TmLam VarName Term
  | TmApp Term Term
  | TmRcd [(Label, Term)]
  | TmSel Term Label

------------------------------------------------------------------------------------------
-- (Mutable) Simple types
-- These should be removed since we want to switch to "french style" HM
------------------------------------------------------------------------------------------

data VariableStateMut = MkVariableStateMut
  { lowerBoundsM :: [SimpleTypeMut]
  , upperBoundsM :: [SimpleTypeMut]
  }

data SimpleTypeMut
  = TyVarM (IORef VariableStateMut)
  | TyPrimM PrimName
  | TyFunM SimpleTypeMut SimpleTypeMut
  | TyRcdM [(Label, SimpleTypeMut)]
  deriving (Eq)

------------------------------------------------------------------------------------------
-- (Immutable) Simple types
------------------------------------------------------------------------------------------

data VariableState = MkVariableState
  { lowerBounds :: [SimpleType]
  , upperBounds :: [SimpleType]
  }
  deriving (Eq, Ord)

data SimpleType
  = TyVar VariableState
  | TyPrim PrimName
  | TyFun SimpleType SimpleType
  | TyRcd [(Label, SimpleType)]
  deriving (Eq, Ord)

freeze :: SimpleTypeMut -> IO (SimpleType)
freeze (TyVarM ref) = do
  vs <- readIORef ref
  lb <- forM (lowerBoundsM vs) freeze
  ub <- forM (upperBoundsM vs) freeze
  return (TyVar (MkVariableState lb ub))
freeze (TyPrimM n) = return (TyPrim n)
freeze (TyFunM t1 t2) = do
  t1f <- freeze t1
  t2f <- freeze t2
  return (TyFun t1f t2f)
freeze (TyRcdM fs) = do
  fsf <- forM fs (\(lbl, t) -> do
                     tf <- freeze t
                     return (lbl,tf))
  return (TyRcd fsf)

------------------------------------------------------------------------------------------
-- Target Types
------------------------------------------------------------------------------------------

data TargetType
  = TTyTop
  | TTyBot
  | TTyUnion TargetType TargetType
  | TTyInter TargetType TargetType
  | TTyFun TargetType TargetType
  | TTyRcd [(Label, TargetType)]
  | TTyRec VarName TargetType
  | TTyVar VarName
  | TTyPrim PrimName

data Polarity = Pos | Neg deriving (Eq, Ord)

switchPol :: Polarity -> Polarity
switchPol Pos = Neg
switchPol Neg = Pos

type PolarVariable = (VariableState, Polarity)


