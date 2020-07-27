module Examples where

import Syntax
import Inference

-- Identity function "\x.x"
idtm :: Term
idtm = TmLam "x" (TmVar "x")

idtmInferred :: SimpleTypeR
idtmInferred = infer idtm

-- Constant function "\x.2"
constFun :: Term
constFun = TmLam "x" (TmLit 2)

constFunInferred :: SimpleTypeR
constFunInferred = infer constFun

-- Application "(\x.x) 2"
appExmpl :: Term
appExmpl = TmApp idtm (TmLit 2)

appExmplInferred :: SimpleTypeR
appExmplInferred = infer appExmpl

-- Selfapplication \x.(xx)
selfApp :: Term
selfApp = TmLam "x" (TmApp (TmVar "x") (TmVar "x"))

selfAppInferred :: SimpleTypeR
selfAppInferred = infer selfApp
