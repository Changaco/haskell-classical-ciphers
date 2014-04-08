module ClassicalCiphers.Common where

import BasicPrelude

import Control.Monad.ST
import Data.Char (ord, toUpper)
import qualified Data.ByteString.Char8 as C
import Data.Vector (freeze)
import qualified Data.Vector.Mutable as MV

import Utils (nubOrd)


class Cipher c where
    type DecipherKeyType c
    type EncipherKeyType c
    type KeysType c a
    cipherName :: c -> Text
    cipherNumberOfKeys :: c -> Int
    makeDecipherKey :: c -> ByteString -> DecipherKeyType c
    makeEncipherKey :: c -> ByteString -> EncipherKeyType c
    decipher :: c -> KeysType c (DecipherKeyType c) -> String -> String
    encipher :: c -> KeysType c (EncipherKeyType c) -> String -> String


type Square = ByteString
type SquareMap = Vector Int


alphabetNoQ :: Square
alphabetNoQ = "ABCDEFGHIJKLMNOPRSTUVWXYZ"


alphabetNoQMap :: SquareMap
alphabetNoQMap = genSquareMap alphabetNoQ


filterNonAlpha :: ByteString -> ByteString
filterNonAlpha = C.filter (\c -> c>='A' && c<='Z') . C.map toUpper


genSquareMap :: Square -> SquareMap
genSquareMap square = runST $ do
    mv <- MV.new 26
    MV.set mv 0
    forM_ (zip [0..] $ C.unpack square) $ \(i, c) -> do
        MV.write mv (ord c - 65) i
    freeze mv


genSquareNoQ :: ByteString -> Square
genSquareNoQ = C.pack . nubOrd . filter (/='Q') . (++['A'..'Z']) . C.unpack . filterNonAlpha
