module ClassicalCiphers.TwosquareSpec where

import Test.Hspec

import ClassicalCiphers.Common
import ClassicalCiphers.Twosquare

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "twosquare" $ do
  it "decipher" $ do
    decipher_ TwosquareVNoQ ("EXAMPLE", "KEYWORD") "HEDLXWSDJYANHOTKDG" `shouldBe` "HELPMEOBIWANKENOBI"
  it "encipher" $ do
    encipher_ TwosquareVNoQ ("EXAMPLE", "KEYWORD") "Help me Obiwan Kenobi" `shouldBe` "HEDLXWSDJYANHOTKDG"
