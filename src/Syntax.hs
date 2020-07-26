module Syntax where

import Data.IORef

type PrimName = String
type VarName = String
type Label = String

data IsRec = Rec | NonRec

data Term
  = TmLit Int
  | TmVar VarName
  | TmLam VarName Term
  | TmApp Term Term
  | TmRcd [(Label, Term)]
  | TmSel Term Label
--  | TmLet IsRec VarName Term Term


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

