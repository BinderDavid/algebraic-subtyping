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

type Ctx = Map VarName SimpleType

type InferenceM a = StateT ([(SimpleType, SimpleType)], Ctx) IO a

runInferenceM :: InferenceM a -> IO a
runInferenceM m = evalStateT m ([], M.empty)

lookupVar :: VarName -> InferenceM SimpleType
lookupVar x = do
  ctx <- gets snd
  return (ctx M.! x)

freshTyVar :: InferenceM SimpleType
freshTyVar = do
  varStateRef <- liftIO $ newIORef (MkVariableState { lowerBounds = [], upperBounds = [] })
  return (TyVar varStateRef)

typeTerm :: Term -> InferenceM SimpleType
typeTerm (TmLit _) = return (TyPrim "Int")
typeTerm (TmVar x) = lookupVar x
typeTerm (TmRcd xs) = do
  inferredTypes <- forM xs (\(lbl, tm) -> typeTerm tm >>= \ty -> return (lbl, ty))
  return (TyRcd inferredTypes)
typeTerm (TmLam var tm) = do
  tyvar <- freshTyVar
  modify (\(cache, ctx) -> (cache, M.insert var tyvar ctx))
  returnType <- typeTerm tm
  return (TyFun tyvar returnType)
typeTerm (TmApp tm1 tm2) = do
  resType <- freshTyVar
  ty1 <- typeTerm tm1
  ty2 <- typeTerm tm2
  constrainMemoized (ty1, TyFun ty2 resType)
  return resType
typeTerm (TmSel tm lbl) = do
  resType <- freshTyVar
  ty <- typeTerm tm
  constrainMemoized (ty, TyRcd [(lbl, resType)])
  return resType


------------------------------------------------------------------------------------------
-- Type Constraining
------------------------------------------------------------------------------------------

constrainMemoized :: (SimpleType, SimpleType) -> InferenceM ()
constrainMemoized arg = do
  cache <- gets fst
  case arg `elem` cache of
    True -> return ()
    False -> do
      modify (\(_,ctx) -> (arg : cache, ctx))
      constrain arg

-- | Force the first type to be a subtype of the second type, otherwise fail.
constrain :: (SimpleType, SimpleType) -> InferenceM ()
constrain (TyPrim n, TyPrim n') | n == n' = return ()
constrain (TyFun argt rest, TyFun argt' rest') = do
  constrainMemoized (argt',argt) -- Contravariant for argument types
  constrainMemoized (rest,rest') -- Covariant for return types
constrain (TyRcd fs0, TyRcd fs1) = do
  forM_ fs1 (\(lbl,ty) -> case lookup lbl fs0 of
                Nothing -> error "Missing field"
                Just ty' -> constrainMemoized (ty', ty))
constrain (TyVar lhs, rhs) = do
  liftIO $ modifyIORef lhs (\vs@(MkVariableState { upperBounds }) -> vs { upperBounds = rhs : upperBounds })
  lowerBs <- liftIO $ lowerBounds <$> readIORef lhs
  forM_ lowerBs (\lb -> constrainMemoized (lb, rhs))
constrain (lhs, TyVar rhs) = do
  liftIO $ modifyIORef rhs (\vs@(MkVariableState { lowerBounds }) -> vs { lowerBounds = lhs : lowerBounds })
  upperBs <- liftIO $ upperBounds <$> readIORef rhs
  forM_ upperBs (\ub -> constrainMemoized (lhs, ub))
constrain (_,_)= error "Cannot constrain"
