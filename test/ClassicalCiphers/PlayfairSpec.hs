module ClassicalCiphers.PlayfairSpec where

import Test.Hspec

import ClassicalCiphers.Common
import ClassicalCiphers.Playfair

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "playfair" $ do
  it "decipher" $ do
    decipher_ PlayfairNoQ "EXAMPLE" "GXFEPXVHRAERGPORCH" `shouldBe` "HELPMEOBIWANKENOBI"
  it "encipher" $ do
    encipher_ PlayfairNoQ "EXAMPLE" "Help me Obiwan Kenobi" `shouldBe` "GXFEPXVHRAERGPORCH"
