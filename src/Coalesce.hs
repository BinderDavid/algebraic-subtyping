{-# LANGUAGE NamedFieldPuns #-}
module Coalesce
  ( coalesceMap
  , printCoalesceMap
  , zonk
  , generalize
  ) where

import Data.List (intersperse)
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

coalesce :: Map UVar VariableState -> Set PolarizedUVar -> Polarity -> SimpleType -> TargetType
coalesce _ _ _ (TyPrim p) = TTyPrim p
coalesce mp cache pol (TyFun ty1 ty2) =
  let
    tty1 = coalesce mp cache (switchPol pol) ty1
    tty2 = coalesce mp cache            pol  ty2
  in
    TTyFun tty1 tty2
coalesce mp cache pol (TyRcd fs) =
  let
    fs' = undefined
  in
    TTyRcd fs'
coalesce mp cache Pos (TyVar uv) =
  case (Pos,uv) `S.member` cache of
    True -> TTyVar (uvarToTVarP uv)
    False ->
      let
        newCache = S.insert (Pos, uv) cache
        lbs = lowerBounds (M.findWithDefault (MkVariableState [] []) uv mp)
        ttlbs = coalesce mp newCache Pos <$> lbs
        ttunion = union (TTyVar (uvarToTVar uv) : ttlbs)
      in
        TTyRec (uvarToTVarP uv) ttunion
coalesce mp cache Neg (TyVar uv) =
  case (Neg,uv) `S.member` cache of
    True -> TTyVar (uvarToTVarN uv)
    False ->
      let
        newCache = S.insert (Neg, uv) cache
        lbs = upperBounds (M.findWithDefault (MkVariableState [] []) uv mp)
        ttlbs = coalesce mp newCache Neg <$> lbs
        ttunion = inter (TTyVar (uvarToTVar uv) : ttlbs)
      in
        TTyRec (uvarToTVarP uv) ttunion

coalesceMap :: Map UVar VariableState -> Map PolarizedUVar TargetType
coalesceMap m =
  let
    elems = M.keys m
    blub uv = [ ((Pos,uv), coalesce m S.empty Pos (TyVar uv))
              , ((Neg,uv), coalesce m S.empty Neg (TyVar uv))]
    blab = concat (blub <$> elems)
  in
    M.fromList blab

printCoalesceMap :: Map PolarizedUVar TargetType -> String
printCoalesceMap m =
  let
    elems = M.assocs m
    printElement ((Pos,var), tty) = "     +" <> printUVar var <> " => " <> printTargetType tty
    printElement ((Neg,var), tty) = "     -" <> printUVar var <> " => " <> printTargetType tty
  in
    concat (intersperse "\n" (printElement <$> elems))

------------------------------------------------------------------------------------------
-- Zonking
------------------------------------------------------------------------------------------

zonk :: Map PolarizedUVar TargetType -> Polarity -> SimpleType -> TargetType
zonk _ _   (TyPrim p) = TTyPrim p
zonk m pol (TyVar uv) = m M.! (pol,uv)
zonk m pol (TyRcd fs) =
  let
    fs' = (\(lbl,ty) -> (lbl, zonk m pol ty)) <$> fs
  in
    TTyRcd fs'
zonk m pol (TyFun ty1 ty2) = TTyFun (zonk m (switchPol pol) ty1) (zonk m pol ty2)

------------------------------------------------------------------------------------------
-- Generalize
------------------------------------------------------------------------------------------

freeTVars :: TargetType -> [TVar]
freeTVars tt = S.elems (freeTVars' tt)
  where
    freeTVars' :: TargetType -> Set TVar
    freeTVars' TTyTop = S.empty
    freeTVars' TTyBot = S.empty
    freeTVars' (TTyUnion tt1 tt2) = S.union (freeTVars' tt1) (freeTVars' tt2)
    freeTVars' (TTyInter tt1 tt2) = S.union (freeTVars' tt1) (freeTVars' tt2)
    freeTVars' (TTyFun tt1 tt2) = S.union (freeTVars' tt1) (freeTVars' tt2)
    freeTVars' (TTyRcd fs) = S.unions ((\(_,tt) -> freeTVars' tt) <$> fs)
    freeTVars' (TTyRec v tt) = S.delete v (freeTVars' tt)
    freeTVars' (TTyVar v) = S.singleton v
    freeTVars' (TTyPrim _) = S.empty

generalize :: TargetType -> TypeScheme
generalize tt = (freeTVars tt, tt)
