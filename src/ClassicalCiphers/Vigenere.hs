module ClassicalCiphers.Vigenere
    ( Vigenere(..)
    ) where

import BasicPrelude

import Data.Char (chr, ord)

import ClassicalCiphers.Common


data Vigenere = Vigenere

instance Cipher Vigenere where
    type DecipherKeyType Vigenere = [Int]
    type EncipherKeyType Vigenere = [Int]
    type KeysType Vigenere a = a
    cipherName _ = "Vigenère"
    cipherNumberOfKeys _ = 1
    makeDecipherKey _ = map (\c -> ord c - 65)
    makeEncipherKey _ = map (\c -> ord c - 65)
    makeDecipherKeys = makeDecipherKey
    makeEncipherKeys = makeEncipherKey
    decipher _ = decipherVigenere
    encipher _ = encipherVigenere


decipherVigenere :: DecipherKeyType Vigenere -> String -> String
decipherVigenere key = go key
  where
    go (k:ks) (c:cs) = chr ((ord c - 65 - k) % 26 + 65) : go ks cs
    go [] l = go key l
    go _ [] = []


encipherVigenere :: EncipherKeyType Vigenere -> String -> String
encipherVigenere key = go key
  where
    go (k:ks) (c:cs) = chr ((ord c - 65 + k) % 26 + 65) : go ks cs
    go [] l = go key l
    go _ [] = []
