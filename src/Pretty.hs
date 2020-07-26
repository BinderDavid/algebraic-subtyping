{-# LANGUAGE NamedFieldPuns #-}
module Pretty where

import Control.Monad (forM)
import Data.IORef
import Data.List (intersperse)

import Syntax

printTerm :: Term -> String
printTerm (TmLit i) = show i
printTerm (TmVar v) = v
printTerm (TmLam x t) = "\\" <> x <> "." <> printTerm t
printTerm (TmApp t1 t2) = "(" <> printTerm t1 <> " " <> printTerm t2 <> ")"
printTerm (TmRcd fs) = "{" <> concat (printFoo <$> fs) <> "}"
  where
    printFoo (lbl, tm) = lbl <> " = " <> printTerm tm <> ","
printTerm (TmSel tm lbl) = printTerm tm <> "." <> lbl


printSimpleType :: SimpleType -> IO String
printSimpleType (TyVar ref) = do
  vs <- readIORef ref
  printVariableState vs
printSimpleType (TyPrim n) = return n
printSimpleType (TyFun t1 t2) = do
  t1p <- printSimpleType t1
  t2p <- printSimpleType t2
  return ("(" <> t1p <> " -> " <> t2p <> ")")
printSimpleType (TyRcd fs) = do
  fsp <- forM fs (\(lbl, ty) -> do
                     typ <- printSimpleType ty
                     return (lbl <> " : " <> typ <> ", "))
  return ("{" <> concat fsp <> "}")

printVariableState :: VariableState -> IO String
printVariableState MkVariableState { lowerBounds, upperBounds } = do
  lbp <- forM lowerBounds printSimpleType
  ubp <- forM upperBounds printSimpleType
  return ("< lower: " <> concat (intersperse "," lbp) <> " upper: " <> concat (intersperse "," ubp) <> " >")
