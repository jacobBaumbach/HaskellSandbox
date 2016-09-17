module Ciphers where

import Data.Char

ensureAlpha :: Int -> Int
ensureAlpha x = (mod (x-97) 26)+97

cipher :: Int -> Char -> Char
cipher f x = (chr . ensureAlpha . (\y -> f+y) . ord) x

caesarCiper :: Int -> String -> String
caesarCiper f x = map (cipher f) x

ident :: Int -> String -> String
ident f x = caesarCiper (-f) (caesarCiper f x)