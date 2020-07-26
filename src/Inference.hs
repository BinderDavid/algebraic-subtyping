{-# LANGUAGE NamedFieldPuns #-}
module Inference where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.IORef

import Syntax


------------------------------------------------------------------------------------------
-- Type Inference
------------------------------------------------------------------------------------------

type Ctx = Map VarName SimpleTypeMut

type InferenceM a = StateT ([(SimpleTypeMut, SimpleTypeMut)], Ctx) IO a

runInferenceM :: InferenceM a -> IO a
runInferenceM m = evalStateT m ([], M.empty)

lookupVar :: VarName -> InferenceM SimpleTypeMut
lookupVar x = do
  ctx <- gets snd
  return (ctx M.! x)

freshTyVar :: InferenceM SimpleTypeMut
freshTyVar = do
  varStateRef <- liftIO $ newIORef (MkVariableStateMut { lowerBoundsM = [], upperBoundsM = [] })
  return (TyVarM varStateRef)

typeTerm :: Term -> InferenceM SimpleTypeMut
typeTerm (TmLit _) = return (TyPrimM "Int")
typeTerm (TmVar x) = lookupVar x
typeTerm (TmRcd xs) = do
  inferredTypes <- forM xs (\(lbl, tm) -> typeTerm tm >>= \ty -> return (lbl, ty))
  return (TyRcdM inferredTypes)
typeTerm (TmLam var tm) = do
  tyvar <- freshTyVar
  modify (\(cache, ctx) -> (cache, M.insert var tyvar ctx))
  returnType <- typeTerm tm
  return (TyFunM tyvar returnType)
typeTerm (TmApp tm1 tm2) = do
  resType <- freshTyVar
  ty1 <- typeTerm tm1
  ty2 <- typeTerm tm2
  constrainMemoized (ty1, TyFunM ty2 resType)
  return resType
typeTerm (TmSel tm lbl) = do
  resType <- freshTyVar
  ty <- typeTerm tm
  constrainMemoized (ty, TyRcdM [(lbl, resType)])
  return resType


------------------------------------------------------------------------------------------
-- Type Constraining
------------------------------------------------------------------------------------------

constrainMemoized :: (SimpleTypeMut, SimpleTypeMut) -> InferenceM ()
constrainMemoized arg = do
  cache <- gets fst
  case arg `elem` cache of
    True -> return ()
    False -> do
      modify (\(_,ctx) -> (arg : cache, ctx))
      constrain arg

-- | Force the first type to be a subtype of the second type, otherwise fail.
constrain :: (SimpleTypeMut, SimpleTypeMut) -> InferenceM ()
constrain (TyPrimM n, TyPrimM n') | n == n' = return ()
constrain (TyFunM argt rest, TyFunM argt' rest') = do
  constrainMemoized (argt',argt) -- Contravariant for argument types
  constrainMemoized (rest,rest') -- Covariant for return types
constrain (TyRcdM fs0, TyRcdM fs1) = do
  forM_ fs1 (\(lbl,ty) -> case lookup lbl fs0 of
                Nothing -> error "Missing field"
                Just ty' -> constrainMemoized (ty', ty))
constrain (TyVarM lhs, rhs) = do
  liftIO $ modifyIORef lhs (\vs@(MkVariableStateMut { upperBoundsM }) -> vs { upperBoundsM = rhs : upperBoundsM })
  lowerBs <- liftIO $ lowerBoundsM <$> readIORef lhs
  forM_ lowerBs (\lb -> constrainMemoized (lb, rhs))
constrain (lhs, TyVarM rhs) = do
  liftIO $ modifyIORef rhs (\vs@(MkVariableStateMut { lowerBoundsM }) -> vs { lowerBoundsM = lhs : lowerBoundsM })
  upperBs <- liftIO $ upperBoundsM <$> readIORef rhs
  forM_ upperBs (\ub -> constrainMemoized (lhs, ub))
constrain (_,_)= error "Cannot constrain"
