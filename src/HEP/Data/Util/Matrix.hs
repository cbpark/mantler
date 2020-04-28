module HEP.Data.Util.Matrix where

newtype FElem p = FElem { evalF :: p -> Double }

instance Num (FElem p) where
    FElem f + FElem g = FElem (\p -> f p + g p)
    FElem f - FElem g = FElem (\p -> f p - g p)
    FElem f * FElem g = FElem (\p -> f p * g p)
    abs (FElem f) = FElem (abs . f)
    signum (FElem f) = FElem (signum . f)
    fromInteger i = FElem (const (fromInteger i))

data Row2 e = Row2 e e deriving Show

data Mat22 e = Mat22 (Row2 e) (Row2 e) deriving Show

-- |
-- >>> let r1 = Row2 (FElem (\x -> x)) (FElem (\x -> x**2 + 1))
-- >>> let r2 = Row2 (FElem (\x -> x**2 + x - 1)) (FElem (\x -> x**3))
-- >>> let m = Mat22 r1 r2
-- >>> (evalF (det m)) 1
-- -1.0
det :: Num e => Mat22 e -> e
det (Mat22 (Row2 a b) (Row2 c d)) = a * d - b * c
