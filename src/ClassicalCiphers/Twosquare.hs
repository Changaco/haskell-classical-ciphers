module ClassicalCiphers.Twosquare where

import BasicPrelude

import qualified Data.ByteString.Char8 as C

import ClassicalCiphers.Common


data Orientation = Horizontal | Vertical deriving (Eq,Show)


twosquare :: Orientation -> Square -> Square -> String -> String
twosquare o k1 k2 (i1:i2:xs) =
    let (row1, col1) = (fromMaybe 0 $ C.elemIndex i1 k1) `divMod` 5
        (row2, col2) = (fromMaybe 0 $ C.elemIndex i2 k2) `divMod` 5
        o1 = C.index (if o == Horizontal then k2 else k1) (5 * row1 + col2)
        o2 = C.index (if o == Horizontal then k1 else k2) (5 * row2 + col1)
    in  o1 : o2 : (twosquare o k1 k2 xs)
twosquare _ _ _ [] = []
twosquare _ _ _ [_] = error "twosquare can only decipher even-lengthed strings"


twosquareh, twosquarev :: Square -> Square -> String -> String
twosquareh = twosquare Horizontal
twosquarev = twosquare Vertical


twosquareAll :: Square -> Square -> String -> [String]
twosquareAll k1 k2 s =
    [ twosquareh k1 k2 s, twosquarev k1 k2 s
    , twosquareh k2 k1 s, twosquarev k2 k1 s
    ]
