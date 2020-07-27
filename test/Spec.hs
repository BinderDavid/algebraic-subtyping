import Test.Hspec

import Parser (parseTerm)
import Syntax

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "\"x\" parses correctly" $ do
      parseTerm "x" `shouldBe` Right (TmVar "x")
    it "\"123\" parses correctly" $ do
      parseTerm "123" `shouldBe` Right (TmLit 123)
    it "\"\\x.x\" parses correctly" $ do
      parseTerm "\\x.x" `shouldBe` Right (TmLam "x" (TmVar "x"))
