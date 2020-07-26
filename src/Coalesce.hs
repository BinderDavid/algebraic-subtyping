module Coalesce where

import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S

import Syntax

------------------------------------------------------------------------------------------
-- Type Coalescing
------------------------------------------------------------------------------------------

coalesceType :: SimpleTypeFrozen -> State (Map PolarVariable ()) TargetType
coalesceType ty = go ty Pos S.empty

go :: SimpleTypeFrozen -> Polarity -> Set PolarVariable -> State (Map PolarVariable ()) TargetType
go (TyPrimF n) _ _ = return (TTyPrim n)
go (TyFunF t1 t2) pol inProcess = do
  tt1 <- go t1 (switchPol pol) inProcess
  tt2 <- go t2 pol inProcess
  return (TTyFun tt1 tt2)
go (TyRcdF fs) pol inProcess = do
  fs' <- forM fs (\(lbl, ty) -> do
                     tty <- go ty pol inProcess
                     return (lbl,tty))
  return (TTyRcd fs')
go (TyVarF vs) pol inProcess = do
  let vs_pol = (vs, pol)
  case S.member vs_pol inProcess of
    True -> do
      recursive <- get
      undefined
    False -> do
      let bounds = case pol of
            Pos -> lowerBoundsF vs
            Neg -> upperBoundsF vs
      boundTypes <- undefined
      let mrg = case pol of
            Pos -> TTyUnion
            Neg -> TTyInter
      let res = undefined
      undefined

