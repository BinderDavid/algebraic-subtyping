{-# LANGUAGE NamedFieldPuns #-}
module Coalesce where

import Data.List (intersperse)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Syntax
import Pretty

------------------------------------------------------------------------------------------
-- Type Coalescing
------------------------------------------------------------------------------------------

union :: [TargetType] -> TargetType
union [] = TTyTop
union [ty] = ty
union (ty:tys) = TTyUnion ty (union tys)

inter :: [TargetType] -> TargetType
inter [] = TTyBot
inter [ty] = ty
inter (ty:tys) = TTyInter ty (inter tys)

bar :: Polarity -> VariableState -> TargetType
bar Pos MkVariableState { lowerBounds } = union (foo Pos <$> lowerBounds)
bar Neg MkVariableState { upperBounds } = inter (foo Neg <$> upperBounds)


foo :: Polarity -> SimpleType -> TargetType
foo _   (TyPrim n) = TTyPrim n
foo pol (TyFun ty1 ty2) = TTyFun (foo (switchPol pol) ty1) (foo pol ty2)
foo pol (TyRcd fs) = TTyRcd ((\(lbl, ty) -> (lbl,foo pol ty)) <$> fs)
foo pol (TyVar v) = TTyVar (pol, v)

coalescePart1 :: Map UVar VariableState -> Map (Polarity, UVar) TargetType
coalescePart1 m =
  let
    elems = M.assocs m
    blub (uv,st) = [((Pos,uv), bar Pos st), ((Neg,uv), bar Neg st)]
    blab = concat (blub <$> elems)
  in
    M.fromList blab

printCoalescePart1 :: Map (Polarity, UVar) TargetType -> String
printCoalescePart1 m =
  let
    elems = M.assocs m
    printElement ((Pos,var), tty) = "     +" <> printUVar var <> " => " <> printTargetType tty
    printElement ((Neg,var), tty) = "     -" <> printUVar var <> " => " <> printTargetType tty
  in
    concat (intersperse "\n" (printElement <$> elems))

