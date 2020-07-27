module Syntax where

data Primitive = PrimInt deriving (Show, Eq, Ord)
type VarName = String
type TyVarName = String
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
-- Simple unresolved types
------------------------------------------------------------------------------------------

data SimpleType
  = TyVar TyVarName
  | TyPrim Primitive
  | TyFun SimpleType SimpleType
  | TyRcd [(Label, SimpleType)]
  deriving (Eq, Ord)

data VariableState = MkVariableState
  { lowerBounds :: [SimpleType]
  , upperBounds :: [SimpleType]
  }
  deriving (Eq, Ord)

------------------------------------------------------------------------------------------
-- Simple resolved types
------------------------------------------------------------------------------------------

data SimpleTypeR
  = TyVarR VariableStateR
  | TyPrimR Primitive
  | TyFunR SimpleTypeR SimpleTypeR
  | TyRcdR [(Label, SimpleTypeR)]
  deriving (Eq, Ord)

data VariableStateR = MkVariableStateR
  { lowerBoundsR :: [SimpleTypeR]
  , upperBoundsR :: [SimpleTypeR]
  }
  deriving (Eq, Ord)

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
  | TTyPrim Primitive

data Polarity = Pos | Neg deriving (Eq, Ord)

switchPol :: Polarity -> Polarity
switchPol Pos = Neg
switchPol Neg = Pos

type PolarVariable = (VariableState, Polarity)


