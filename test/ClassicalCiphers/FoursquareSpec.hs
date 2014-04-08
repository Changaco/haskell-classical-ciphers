module ClassicalCiphers.FoursquareSpec where

import Test.Hspec

import ClassicalCiphers.Common
import ClassicalCiphers.Foursquare

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "foursquare" $ do
  it "decipher" $ do
    decipher_ FoursquareNoQ ("EXAMPLE", "KEYWORD") "FYGMKYHOBXMFKKKIMD" `shouldBe` "HELPMEOBIWANKENOBI"
  it "encipher" $ do
    encipher_ FoursquareNoQ ("EXAMPLE", "KEYWORD") "Help me Obiwan Kenobi" `shouldBe` "FYGMKYHOBXMFKKKIMD"
