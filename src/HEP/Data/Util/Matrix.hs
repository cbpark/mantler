module HEP.Data.Util.Matrix where

newtype FElem = FElem { evalF :: Double -> Double }

instance Num FElem where
    FElem f + FElem g = FElem (\x -> f x + g x)
    FElem f - FElem g = FElem (\x -> f x - g x)
    FElem f * FElem g = FElem (\x -> f x * g x)
    abs (FElem f) = FElem (abs . f)
    signum (FElem f) = FElem (signum . f)
    fromInteger i = FElem (const (fromInteger i))

data Row2 a = Row2 a a deriving Show

data Mat22 a = Mat22 (Row2 a) (Row2 a) deriving Show

-- |
-- >>> let r1 = Row2 (FElem (\x -> x)) (FElem (\x -> x**2 + 1))
-- >>> let r2 = Row2 (FElem (\x -> x**2 + x - 1)) (FElem (\x -> x**3))
-- >>> let m = Mat22 r1 r2
-- >>> (evalF (det m)) 1
-- -1.0
det :: Num a => Mat22 a -> a
det (Mat22 (Row2 a b) (Row2 c d)) = a * d - b * c
