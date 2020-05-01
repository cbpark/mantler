module HEP.Util.Matrix where

data Row2 e = Row2 !e !e deriving Show

data Mat22 e = Mat22 (Row2 e) (Row2 e) deriving Show

det :: Num e => Mat22 e -> e
det (Mat22 (Row2 a b) (Row2 c d)) = a * d - b * c
