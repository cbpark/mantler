{-# LANGUAGE Strict #-}

module HEP.Util.Polynomial (quarticEqSol, quadEqSol) where

import Numeric.GSL.Polynomials (polySolve)

import Data.Complex            (imagPart, realPart)
import Data.List               (nub, sort)

-- | returns the real roots of quartic polynomial equation in ascending order.
--
-- It does not return zero roots. Thus, if the roots are empty, it implies
-- that the polynomial has only zero roots.
quarticEqSol :: (Double -> Double)
             -> [Double]               -- ^ four distinct nonzero inputs
             -> Double                 -- ^ the cut value for complex roots
             -> Maybe (Int, [Double])  -- ^ (the number of real roots, roots)
quarticEqSol f xs eps
    | length (filter (/= 0) xs) /= 4 || xs /= nub xs = Nothing
    | otherwise = do
          let roots = filter (\r -> abs (imagPart r) < eps) $
                      polySolve (coeffQuartic f xs)
              realroots = (sort . filter ((> eps) . abs)) $ realPart <$> roots
          return (length realroots, realroots)

-- | returns the coefficients of quartic polynomial
--   using four distinct nonzero inputs.
--
--   a * x^4 + b * x^3 + c * x^2 + d * x + e
--
--   the output is the list of the coefficients [e, d, c, b, a].
coeffQuartic :: (Double -> Double) -> [Double] -> [Double]
coeffQuartic f xs =
    let [x1, x2, x3, x4] = xs
        [f1, f2, f3, f4] = f <$> xs

        d1 = x1 * (x2 - x1) * (x3 - x1) * (x4 - x1)
        d2 = x2 * (x2 - x1) * (x3 - x2) * (x4 - x2)
        d3 = x3 * (x3 - x1) * (x3 - x2) * (x4 - x3)
        d4 = x4 * (x4 - x1) * (x4 - x2) * (x4 - x3)

        p12 = x1 * x2
        p13 = x1 * x3
        p14 = x1 * x4
        p23 = x2 * x3
        p24 = x2 * x4
        p34 = x3 * x4
        p1234 = p12 * p34

        e = f 0

        a = - f1 / d1 + f2 / d2 - f3 / d3 + f4 / d4 + e / p1234

        b =   f1 * (x2 + x3 + x4) / d1
            - f2 * (x4 + x3 + x1) / d2
            + f3 * (x4 + x2 + x1) / d3
            - f4 * (x3 + x2 + x1) / d4
            - e * (x1 + x2 + x3 + x4) / p1234

        c = - f1 * (p34 + p24 + p23) / d1
            + f2 * (p34 + p14 + p13) / d2
            - f3 * (p24 + p14 + p12) / d3
            + f4 * (p23 + p13 + p12) / d4
            + e * (p34 + p24 + p14 + p23 + p13 + p12) / p1234

        d =   f1 * x2 * p34 / d1
            - f2 * x3 * p14 / d2
            + f3 * x4 * p12 / d3
            - f4 * x1 * p23 / d4
            - e * (x2 * p34 + x3 * p14 + x4 * p12 + x1 * p23) / p1234
    in
        [e, d, c, b, a]

-- | the real roots of quadratic equation: a x^2 + b x + c = 0.
--
-- If the root x0 is complex and |Im(x0)| < cut, it returns Re(x0).
quadEqSol :: Double  -- ^ the coefficient a
          -> Double  -- ^ the coefficient b
          -> Double  -- ^ the coefficient c
          -> Double  -- ^ the cut value for complex roots
          -> Maybe (Double, Double)
quadEqSol a b c epsScale = do
    let d = b * b - 4 * a * c
        eps = 1.0e-12
    if d >= 0
        -- then return ((-b + sqrt d) / (2*a), (-b - sqrt d) / (2*a))
        then do let q = -0.5 * (b + signum b * sqrt d)
                return (q / (a + eps), c / (q  + eps))
        else do let r = sqrt $ c / (a + eps)
                    th = acos $ - b / (2 * sqrt (a * c) + eps)
                    x = r * cos th
                if abs (r * sin th) < epsScale
                    then return (x, x)
                    else Nothing
