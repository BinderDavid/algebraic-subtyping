import Test.Hspec

import Control.Monad (forM_)
import Parser (parseTerm)
import Syntax

testPairs :: [(String, Term)]
testPairs = [ ("x", TmVar "x")
            , ("123", TmLit 123)
            , ("\\x.x", TmLam "x" (TmVar "x"))
            , ("(x y)", TmApp (TmVar "x") (TmVar "y"))
            ]

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    forM_ testPairs (\(str, res) -> do
                       it ("\"" <> str <> "\" parses correctly") $ do
                         parseTerm str `shouldBe` Right res)

