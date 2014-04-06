module ClassicalCiphers.Foursquare where

import BasicPrelude

import qualified Data.ByteString.Char8 as C
import Data.Char (ord)
import Data.Vector ((!))

import ClassicalCiphers.Common






foursquareE :: Square -> Square -> String -> String
foursquareE k1 k2 (i1:i2:rest) =
    let (row1, col1) = (ord i1 - 65) `divMod` 5
        (row2, col2) = (ord i2 - 65) `divMod` 5
        o1 = C.index k1 (5 * row1 + col2)
        o2 = C.index k2 (5 * row2 + col1)
    in  o1 : o2 : (foursquareE k1 k2 rest)
foursquareE _ _ [] = []
foursquareE _ _ [_] = error "foursquare can only encipher even-lengthed strings"


foursquareD :: SquareMap -> SquareMap -> String -> String
foursquareD k1 k2 (i1:i2:rest) =
    let (row1, col1) = (k1 ! (ord i1 - 65)) `divMod` 5
        (row2, col2) = (k2 ! (ord i2 - 65)) `divMod` 5
        o1 = C.index alphabetNoQ (5 * row1 + col2)
        o2 = C.index alphabetNoQ (5 * row2 + col1)
    in  o1 : o2 : (foursquareD k1 k2 rest)
foursquareD _ _ [] = []
foursquareD _ _ [_] = error "foursquare can only decipher even-lengthed strings"
