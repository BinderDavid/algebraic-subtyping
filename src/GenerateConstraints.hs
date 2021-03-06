{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module GenerateConstraints
  ( Constraint(..)
  , generateConstraints
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Bifunctor (bimap)

import Data.Map (Map)
import qualified Data.Map as M

import Syntax

------------------------------------------------------------------------------------------
-- Constraint generation
------------------------------------------------------------------------------------------

data Constraint = SubType SimpleType SimpleType deriving (Eq, Show)
type Error = String

-- During constraint generation we need:
-- - ReaderT for the local variable context.
-- - WriterT for collecting the generated constraints.
-- - StateT for generating fresh unification variables.
-- - Except for throwing errors when a free variable is encountered.
newtype GenerateM a = MkGenerateM
  { unGenerateM :: ReaderT (Map VarName SimpleType)(WriterT [Constraint](StateT Int (Except Error))) a
  } deriving (Functor, Applicative, Monad, MonadState Int, MonadReader (Map VarName SimpleType), MonadError Error, MonadWriter [Constraint])


runGenerateM :: forall a. GenerateM a -> Either Error (a, [Constraint], [UVar])
runGenerateM m =
  let
    res :: Either Error ((a, [Constraint]), Int)
    res = runExcept (runStateT (runWriterT (runReaderT  (unGenerateM m) M.empty)) 0)
  in
    bimap id (\((x,y),z) -> (x,y,MkUVar <$> [0..(z-1)])) res

-- | Generate a fresh unification variable.
freshUVar :: GenerateM UVar
freshUVar = do
  gen <- get
  modify (+1)
  return (MkUVar gen)

-- | Lookup the type of a variable from the local context.
lookupVar :: VarName -> GenerateM SimpleType
lookupVar v = do
  ctx <- ask
  case M.lookup v ctx of
    Nothing -> throwError ("Free variable error: Variable " <> v <> " is not in the context")
    Just ty -> return ty

-- | Add a subtype constraint.
addConstraint :: SimpleType -> SimpleType -> GenerateM ()
addConstraint ty1 ty2 = tell [SubType ty1 ty2]

-- | Generate the SimpleType of a term, which includes unresolved unification variables.
-- This also generates a set of unification variables and constraints them.
typeTerm :: Term -> GenerateM SimpleType
typeTerm (TmLit _) = return (TyPrim PrimInt)
typeTerm (TmVar v) = lookupVar v
typeTerm (TmLam var tm) = do
  tyvar <- TyVar <$> freshUVar
  ty <- local (\ctx -> M.insert var tyvar ctx) (typeTerm tm)
  return (TyFun tyvar ty)
typeTerm (TmApp tm1 tm2) = do
  ty1 <- typeTerm tm1
  ty2 <- typeTerm tm2
  tyVarRes <- TyVar <$> freshUVar
  addConstraint ty1 (TyFun ty2 tyVarRes)
  return tyVarRes
typeTerm (TmRcd fs) = do
  fs' <- forM fs (\(lbl, tm) -> do
                     ty <- typeTerm tm
                     return (lbl, ty))
  return (TyRcd fs')
typeTerm (TmSel tm lbl) = do
  tyvar <- TyVar <$> freshUVar
  ty <- typeTerm tm
  addConstraint ty (TyRcd [(lbl, tyvar)])
  return tyvar


generateConstraints :: Term -> Either Error (SimpleType, [Constraint], [UVar])
generateConstraints tm = runGenerateM (typeTerm tm)


