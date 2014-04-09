module ClassicalCiphers.Playfair
    ( PlayfairNoQ(..)
    ) where

import BasicPrelude

import qualified Data.ByteString.Char8 as C

import ClassicalCiphers.Common


data PlayfairNoQ = PlayfairNoQ

instance Cipher PlayfairNoQ where
    type DecipherKeyType PlayfairNoQ = Square
    type EncipherKeyType PlayfairNoQ = Square
    type KeysType PlayfairNoQ a = a
    cipherName _ = "Playfair (no Q)"
    cipherNumberOfKeys _ = 1
    makeDecipherKey _ = genSquareNoQ
    makeEncipherKey _ = genSquareNoQ
    makeDecipherKeys = makeDecipherKey
    makeEncipherKeys = makeEncipherKey
    decipher _ = decipherNoQ
    encipher _ = encipherNoQ


decipherNoQ :: Square -> String -> String
decipherNoQ _ (c1:c2:_) | c1 == c2 =
    error (c1:c2:" is not a valid playfair digraph")
decipherNoQ k (c1:c2:rest) =
    let i1 = (fromMaybe 0 $ C.elemIndex c1 k)
        i2 = (fromMaybe 0 $ C.elemIndex c2 k)
        (row1, col1) = i1 `divMod` 5
        (row2, col2) = i2 `divMod` 5
        (j1, j2) = case () of
            _ | row1 == row2 -> (5 * row1 + (col1 - 1) % 5,
                                 5 * row1 + (col2 - 1) % 5)
            _ | col1 == col2 -> ((i1 - 5) % 25,
                                 (i2 - 5) % 25)
            _ | otherwise -> (5 * row1 + col2,
                              5 * row2 + col1)
    in  (C.index k j1) : (C.index k j2) : (decipherNoQ k rest)
decipherNoQ _ [] = []
decipherNoQ _ [_] = error "playfair can only decipher even-lengthed strings"


encipherNoQ :: Square -> String -> String
encipherNoQ k (c1:c2:rest) | c1 == c2 = encipherNoQ k (c1:'Q':c2:rest)
encipherNoQ k (c1:c2:rest) =
    let i1 = (fromMaybe 0 $ C.elemIndex c1 k)
        i2 = (fromMaybe 0 $ C.elemIndex c2 k)
        (row1, col1) = i1 `divMod` 5
        (row2, col2) = i2 `divMod` 5
        (j1, j2) = case () of
            _ | row1 == row2 -> (5 * row1 + (col1 + 1) % 5,
                                 5 * row1 + (col2 + 1) % 5)
            _ | col1 == col2 -> ((i1 + 5) % 25,
                                 (i2 + 5) % 25)
            _ | otherwise -> (5 * row1 + col2,
                              5 * row2 + col1)
    in  (C.index k j1) : (C.index k j2) : (encipherNoQ k rest)
encipherNoQ _ [] = []
encipherNoQ k [c] = encipherNoQ k [c, 'Q']
