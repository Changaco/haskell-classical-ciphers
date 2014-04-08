module ClassicalCiphers.Foursquare
    ( FoursquareNoQ(..)
    ) where

import BasicPrelude

import qualified Data.ByteString.Char8 as C
import Data.Char (ord)
import Data.Vector ((!))

import ClassicalCiphers.Common


data FoursquareNoQ = FoursquareNoQ

instance Cipher FoursquareNoQ where
    type DecipherKeyType FoursquareNoQ = SquareMap
    type EncipherKeyType FoursquareNoQ = Square
    type KeysType FoursquareNoQ a = (a, a)
    cipherName _ = "Foursquare (no Q)"
    cipherNumberOfKeys _ = 2
    makeDecipherKey _ = genSquareMap . genSquareNoQ
    makeEncipherKey _ = genSquareNoQ
    makeDecipherKeys c (k1, k2) = (makeDecipherKey c k1, makeDecipherKey c k2)
    makeEncipherKeys c (k1, k2) = (makeEncipherKey c k1, makeEncipherKey c k2)
    decipher _ = decipherNoQ
    encipher _ = encipherNoQ


decipherNoQ :: (SquareMap, SquareMap) -> String -> String
decipherNoQ (k1, k2) (i1:i2:rest) =
    let (row1, col1) = (k1 ! (ord i1 - 65)) `divMod` 5
        (row2, col2) = (k2 ! (ord i2 - 65)) `divMod` 5
        o1 = C.index alphabetNoQ (5 * row1 + col2)
        o2 = C.index alphabetNoQ (5 * row2 + col1)
    in  o1 : o2 : (decipherNoQ (k1, k2) rest)
decipherNoQ _ [] = []
decipherNoQ _ [_] = error "foursquare can only decipher even-lengthed strings"


encipherNoQ :: (Square, Square) -> String -> String
encipherNoQ (k1, k2) (i1:i2:rest) =
    let (row1, col1) = (alphabetNoQMap ! (ord i1 - 65)) `divMod` 5
        (row2, col2) = (alphabetNoQMap ! (ord i2 - 65)) `divMod` 5
        o1 = C.index k1 (5 * row1 + col2)
        o2 = C.index k2 (5 * row2 + col1)
    in  o1 : o2 : (encipherNoQ (k1, k2) rest)
encipherNoQ _ [] = []
encipherNoQ _ [_] = error "foursquare can only encipher even-lengthed strings"
