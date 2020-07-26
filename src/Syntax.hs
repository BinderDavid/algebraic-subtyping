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
--  | TmLet IsRec VarName Term Term

------------------------------------------------------------------------------------------
-- (Mutable) Simple types
------------------------------------------------------------------------------------------

data VariableState = MkVariableState
  { lowerBounds :: [SimpleType]
  , upperBounds :: [SimpleType]
  }

data SimpleType
  = TyVar (IORef VariableState)
  | TyPrim PrimName
  | TyFun SimpleType SimpleType
  | TyRcd [(Label, SimpleType)]
  deriving (Eq)

------------------------------------------------------------------------------------------
-- (Immutable) Simple types
------------------------------------------------------------------------------------------

data VariableStateFrozen = MkVariableStateFrozen
  { lowerBoundsF :: [SimpleTypeFrozen]
  , upperBoundsF :: [SimpleTypeFrozen]
  }
  deriving (Eq, Ord)

data SimpleTypeFrozen
  = TyVarF VariableStateFrozen
  | TyPrimF PrimName
  | TyFunF SimpleTypeFrozen SimpleTypeFrozen
  | TyRcdF [(Label, SimpleTypeFrozen)]
  deriving (Eq, Ord)

freeze :: SimpleType -> IO (SimpleTypeFrozen)
freeze (TyVar ref) = do
  vs <- readIORef ref
  lb <- forM (lowerBounds vs) freeze
  ub <- forM (upperBounds vs) freeze
  return (TyVarF (MkVariableStateFrozen lb ub))
freeze (TyPrim n) = return (TyPrimF n)
freeze (TyFun t1 t2) = do
  t1f <- freeze t1
  t2f <- freeze t2
  return (TyFunF t1f t2f)
freeze (TyRcd fs) = do
  fsf <- forM fs (\(lbl, t) -> do
                     tf <- freeze t
                     return (lbl,tf))
  return (TyRcdF fsf)

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

type PolarVariable = (VariableStateFrozen, Polarity)


