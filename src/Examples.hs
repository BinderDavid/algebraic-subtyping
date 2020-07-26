module Examples where

import Syntax

-- Identity function "\x.x"
idtm :: Term
idtm = TmLam "x" (TmVar "x")

-- idtmInferred :: IO SimpleTypeMut
-- idtmInferred = runInferenceM (typeTerm idtm)

-- Constant function "\x.2"
constFun :: Term
constFun = TmLam "x" (TmLit 2)

-- constFunInferred :: IO SimpleTypeMut
-- constFunInferred = runInferenceM (typeTerm constFun)

-- Application "(\x.x) 2"
appExmpl :: Term
appExmpl = TmApp idtm (TmLit 2)

-- appExmplInferred :: IO SimpleTypeMut
-- appExmplInferred = runInferenceM (typeTerm appExmpl)

-- Selfapplication \x.(xx)
selfApp :: Term
selfApp = TmLam "x" (TmApp (TmVar "x") (TmVar "x"))

-- BUG!
-- selfAppInferred :: IO SimpleTypeMut
-- selfAppInferred = runInferenceM (typeTerm selfApp)
