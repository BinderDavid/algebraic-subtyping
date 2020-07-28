module Syntax where

data Primitive = PrimInt deriving (Show, Eq, Ord)
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
  deriving (Show, Eq)

------------------------------------------------------------------------------------------
-- Simple unresolved types
------------------------------------------------------------------------------------------

-- | Unification Variable
newtype UVar = MkUVar { uvar_name :: Int } deriving (Eq, Ord, Show)

data SimpleType
  = TyVar UVar
  | TyPrim Primitive
  | TyFun SimpleType SimpleType
  | TyRcd [(Label, SimpleType)]
  deriving (Eq, Ord, Show)

data VariableState = MkVariableState
  { lowerBounds :: [SimpleType]
  , upperBounds :: [SimpleType]
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
  | TTyVar (Polarity, UVar)
  | TTyPrim Primitive
  deriving Show

data Polarity = Pos | Neg deriving (Eq, Ord, Show)

switchPol :: Polarity -> Polarity
switchPol Pos = Neg
switchPol Neg = Pos

