{-# LANGUAGE NamedFieldPuns #-}
module Pretty where

import Control.Monad (forM)
import Data.IORef
import Data.List (intersperse)

import Syntax

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
-- Print mutable simple types
------------------------------------------------------------------------------------------

printSimpleTypeMut :: SimpleTypeMut -> IO String
printSimpleTypeMut (TyVarM ref) = do
  vs <- readIORef ref
  printVariableState vs
printSimpleTypeMut (TyPrimM n) = return n
printSimpleTypeMut (TyFunM t1 t2) = do
  t1p <- printSimpleTypeMut t1
  t2p <- printSimpleTypeMut t2
  return ("(" <> t1p <> " -> " <> t2p <> ")")
printSimpleTypeMut (TyRcdM fs) = do
  fsp <- forM fs (\(lbl, ty) -> do
                     typ <- printSimpleTypeMut ty
                     return (lbl <> " : " <> typ <> ", "))
  return ("{" <> concat fsp <> "}")

printVariableState :: VariableStateMut -> IO String
printVariableState MkVariableStateMut { lowerBoundsM, upperBoundsM } = do
  lbp <- forM lowerBoundsM printSimpleTypeMut
  ubp <- forM upperBoundsM printSimpleTypeMut
  return ("< lower: " <> concat (intersperse "," lbp) <> " upper: " <> concat (intersperse "," ubp) <> " >")
