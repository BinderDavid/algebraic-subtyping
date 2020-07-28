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

data Polarity = Pos | Neg deriving (Eq, Ord, Show)

switchPol :: Polarity -> Polarity
switchPol Pos = Neg
switchPol Neg = Pos

-- | Unification Variable
newtype UVar = MkUVar { uvar_name :: Int } deriving (Eq, Ord, Show)

type PolarizedUVar = (Polarity, UVar)

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

newtype TVar = MkTVar { tvar_name :: String } deriving (Eq, Show, Ord)

uvarToTVar :: UVar -> TVar
uvarToTVar (MkUVar i) = MkTVar ("U" <> show i)

uvarToTVarP :: UVar -> TVar
uvarToTVarP (MkUVar i) = MkTVar ("P" <> show i)

uvarToTVarN :: UVar -> TVar
uvarToTVarN (MkUVar i) = MkTVar ("N" <> show i)

data TargetType
  = TTyTop
  | TTyBot
  | TTyUnion TargetType TargetType
  | TTyInter TargetType TargetType
  | TTyFun TargetType TargetType
  | TTyRcd [(Label, TargetType)]
  | TTyRec TVar TargetType
  | TTyVar TVar
  | TTyPrim Primitive
  deriving Show

------------------------------------------------------------------------------------------
-- Type schemes
------------------------------------------------------------------------------------------

type TypeScheme = ([TVar], TargetType)

