import Test.Hspec

import Control.Monad (forM_)
import Data.Either (isLeft)
import Parser (parseTerm)
import Syntax
import GenerateConstraints

testPairs :: [(String, Term)]
testPairs = [ -- Variants of "x"
              ("x",     TmVar "x")
            , ("(x)",   TmVar "x")
            , (" x ",   TmVar "x")
            , (" (x) ", TmVar "x")
              -- Variants of "123"
            , ("123",   TmLit 123)
            , (" 123 ", TmLit 123)
              -- Variants of "(x y)"
            , ("(x y)",    TmApp (TmVar "x") (TmVar "y"))
            , ("x y",      TmApp (TmVar "x") (TmVar "y"))
            , ("((x y))",  TmApp (TmVar "x") (TmVar "y"))
            , ("(x) (y)",  TmApp (TmVar "x") (TmVar "y"))
            , ("((x)(y))", TmApp (TmVar "x") (TmVar "y"))
              -- Variants of "\x.x"
            , ("\\x.x",   TmLam "x" (TmVar "x"))
            , ("\\x.  x", TmLam "x" (TmVar "x"))
            , ("\\x .x",  TmLam "x" (TmVar "x"))
              -- Variants of "{}"
            , ("{}",    TmRcd [])
            , ("{ }",   TmRcd [])
            , (" { } ", TmRcd [])
              -- Variants of "{ lbl = 5 }"
            , ("{lbl=5}",       TmRcd [("lbl", TmLit 5)])
            , (" { lbl = 5 }",  TmRcd [("lbl", TmLit 5)])
              -- Variants of " { lbl1 = x, lbl2 = f y } "
            , ("{lbl1=x, lbl2=f y}",        TmRcd [("lbl1", TmVar "x"), ("lbl2", TmApp (TmVar "f") (TmVar "y"))])
            , (" { lbl1 = x , lbl2=f y}",   TmRcd [("lbl1", TmVar "x"), ("lbl2", TmApp (TmVar "f") (TmVar "y"))])
            , ("{lbl1=x, lbl2= (f y)}",     TmRcd [("lbl1", TmVar "x"), ("lbl2", TmApp (TmVar "f") (TmVar "y"))])
              -- Variants of "x.lbl"
            , ("x.lbl",     TmSel (TmVar "x") "lbl")
            , (" x.lbl ",   TmSel (TmVar "x") "lbl")
            , (" x . lbl ", TmSel (TmVar "x") "lbl")
            -- Variants of "{ t = 5 }.t"
            , ("{t=5}.t",       TmSel (TmRcd [("t",TmLit 5)]) "t")
            , (" { t = 5 }.t ", TmSel (TmRcd [("t",TmLit 5)]) "t")
            , ("{t=5} . t",     TmSel (TmRcd [("t",TmLit 5)]) "t")
            -- Variants of "(f x).t"
            , ("(f x).t",     TmSel (TmApp (TmVar "f") (TmVar "x")) "t")
            , ("((f x)).t",   TmSel (TmApp (TmVar "f") (TmVar "x")) "t")
            , ("((f x) . t)", TmSel (TmApp (TmVar "f") (TmVar "x")) "t")
            ]

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    forM_ testPairs (\(str, res) -> do
                       it ("\"" <> str <> "\" parses correctly") $ do
                         parseTerm str `shouldBe` Right res)
  describe "Constraint Generation" $ do
    it "x generates an error" $ do
      generateConstraints (TmVar "x") `shouldSatisfy` isLeft
    it "\\x.y generates an error" $ do
      generateConstraints (TmLam "x" (TmVar "y")) `shouldSatisfy` isLeft
    it "(\\x.2) x generates an error" $ do
      generateConstraints (TmApp (TmLam "x" (TmLit 2)) (TmVar "x")) `shouldSatisfy` isLeft

