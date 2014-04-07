module ClassicalCiphers.Twosquare
    ( TwosquareHNoQ(..)
    , TwosquareVNoQ(..)
    ) where

import BasicPrelude

import qualified Data.ByteString.Char8 as C

import ClassicalCiphers.Common


data Orientation = Horizontal | Vertical deriving (Eq,Show)

data TwosquareHNoQ = TwosquareHNoQ

instance Cipher TwosquareHNoQ where
    type DecipherKeyType TwosquareHNoQ = Square
    type EncipherKeyType TwosquareHNoQ = Square
    type KeysType TwosquareHNoQ a = (a, a)
    cipherName _ = "Twosquare (horizontal, no Q)"
    cipherNumberOfKeys _ = 2
    makeDecipherKey _ = genSquareNoQ
    makeEncipherKey _ = genSquareNoQ
    decipher _ = twosquareNoQ Horizontal
    encipher _ = twosquareNoQ Horizontal

data TwosquareVNoQ = TwosquareVNoQ

instance Cipher TwosquareVNoQ where
    type DecipherKeyType TwosquareVNoQ = Square
    type EncipherKeyType TwosquareVNoQ = Square
    type KeysType TwosquareVNoQ a = (a, a)
    cipherName _ = "Twosquare (vertical, no Q)"
    cipherNumberOfKeys _ = 2
    makeDecipherKey _ = genSquareNoQ
    makeEncipherKey _ = genSquareNoQ
    decipher _ = twosquareNoQ Vertical
    encipher _ = twosquareNoQ Vertical


twosquareNoQ :: Orientation -> (Square, Square) -> String -> String
twosquareNoQ o (k1, k2) (i1:i2:xs) =
    let (row1, col1) = (fromMaybe 0 $ C.elemIndex i1 k1) `divMod` 5
        (row2, col2) = (fromMaybe 0 $ C.elemIndex i2 k2) `divMod` 5
        o1 = C.index (if o == Horizontal then k2 else k1) (5 * row1 + col2)
        o2 = C.index (if o == Horizontal then k1 else k2) (5 * row2 + col1)
    in  o1 : o2 : (twosquareNoQ o (k1, k2) xs)
twosquareNoQ _ _ [] = []
twosquareNoQ _ _ [_] = error "twosquare can only decipher even-lengthed strings"
