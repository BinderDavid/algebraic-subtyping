module Coalesce where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Syntax

------------------------------------------------------------------------------------------
-- Type Coalescing
------------------------------------------------------------------------------------------

data CoalescingState = MkCoalescingState
  { inProcess :: Set VariableState
  , recursive :: Map (Polarity, UVar) ()
  }

startingState :: CoalescingState
startingState = MkCoalescingState { inProcess = S.empty, recursive = M.empty }

-- Before:
--
-- U1 => < lower: Ss, upper: Ts >
--
-- After:
--
-- (U1,Pos) => Union (go Ss)
-- (U1,Neg) => Inter (go Ts)
coalesceTypes :: Map UVar VariableState
              -> State CoalescingState (Map (Polarity, UVar) TargetType)
coalesceTypes partialResult = undefined

-- coalesceType :: SimpleType -> State (Map PolarVariable ()) TargetType
-- coalesceType ty = go ty Pos S.empty

-- go :: SimpleType -> Polarity -> Set PolarVariable -> State (Map PolarVariable ()) TargetType
-- go (TyPrim n) _ _ = return (TTyPrim n)
-- go (TyFun t1 t2) pol inProcess = do
--   tt1 <- go t1 (switchPol pol) inProcess
--   tt2 <- go t2 pol inProcess
--   return (TTyFun tt1 tt2)
-- go (TyRcd fs) pol inProcess = do
--   fs' <- forM fs (\(lbl, ty) -> do
--                      tty <- go ty pol inProcess
--                      return (lbl,tty))
--   return (TTyRcd fs')
-- go (TyVar vs) pol inProcess = do
--   let vs_pol = (vs, pol)
--   case S.member vs_pol inProcess of
--     True -> do
--       recursive <- get
--       undefined
--     False -> do
--       let bounds = case pol of
--             Pos -> lowerBounds vs
--             Neg -> upperBounds vs
--       boundTypes <- undefined
--       let mrg = case pol of
--             Pos -> TTyUnion
--             Neg -> TTyInter
--       let res = undefined
--       undefined

