module ClassicalCiphers.VigenereSpec where

import Test.Hspec

import ClassicalCiphers.Common
import ClassicalCiphers.Vigenere

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "playfair" $ do
  it "decipher" $ do
    decipher_ Vigenere "EXAMPLE" "LBLBBPSFFWMCVIRLBU" `shouldBe` "HELPMEOBIWANKENOBI"
  it "encipher" $ do
    encipher_ Vigenere "EXAMPLE" "Help me Obiwan Kenobi" `shouldBe` "LBLBBPSFFWMCVIRLBU"
