import Test.Hspec

import Control.Monad (forM_)
import Parser (parseTerm)
import Syntax

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
            , ("\\x.x", TmLam "x" (TmVar "x"))
            , ("\\x.  x", TmLam "x" (TmVar "x"))
            , ("\\x .x", TmLam "x" (TmVar "x"))
            ]

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    forM_ testPairs (\(str, res) -> do
                       it ("\"" <> str <> "\" parses correctly") $ do
                         parseTerm str `shouldBe` Right res)

