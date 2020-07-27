module Examples where

import Syntax
import Inference

-- Identity function "\x.x"
idtm :: Term
idtm = TmLam "x" (TmVar "x")

-- Constant function "\x.2"
constFun :: Term
constFun = TmLam "x" (TmLit 2)

-- Application "(\x.x) 2"
appExmpl :: Term
appExmpl = TmApp idtm (TmLit 2)

-- Selfapplication \x.(xx)
selfApp :: Term
selfApp = TmLam "x" (TmApp (TmVar "x") (TmVar "x"))

