{-# LANGUAGE RecordWildCards #-}

module HEP.Kinematics.Antler where

import HEP.Kinematics
import HEP.Kinematics.Vector.LorentzVector (setXYZT)

-- import Debug.Trace

data Antler = Antler { _M0sq  :: !Double        -- ^ m_C^2
                     , _M1sq  :: !Double        -- ^ m_B^2
                     , _mV1sq :: !Double        -- ^ p1^2 = m_{v1}^2
                     , _mV2sq :: !Double        -- ^ p2^2 = m_{v2}^2
                     , _v1    :: !FourMomentum  -- ^ p1
                     , _v2    :: !FourMomentum  -- ^ p2
                     , _v1v2  :: !Double        -- ^ p1 . p2
                     } deriving Show

mkAntler :: Double -> Double -> Visibles -> Maybe Antler
mkAntler m0 m1 vis =
    case vis of
        NE             -> Nothing
        Visibles p1 p2 -> do let (mV1, mV2) = (mass p1, mass p2)
                             return $ Antler { _M0sq  = m0 * m0
                                             , _M1sq  = m1 * m1
                                             , _mV1sq = mV1 * mV1
                                             , _mV2sq = mV2 * mV2
                                             , _v1    = p1
                                             , _v2    = p2
                                             , _v1v2  = p1 `dot` p2 }

data Visibles = NE
              | Visibles
                { _p1 :: !FourMomentum
                , _p2 :: !FourMomentum }
              deriving Show

visibles :: HasFourMomentum p => [p] -> Visibles
visibles ps | length ps == 2 = let [p1, p2] = ps
                               in Visibles { _p1 = fourMomentum p1
                                           , _p2 = fourMomentum p2 }
            | otherwise      = NE

data AT = AT { _sAT  :: !Double  -- ^ sigma_{AT}
             , _mAT1 :: !Double  -- ^ min(M_{AT})
             , _mAT2 :: !Double  -- ^ max(M_{AT})
             } deriving Show

calcAT :: Antler
       -> Double  -- ^ - p_{x} component of the ISR
       -> Double  -- ^ - p_{y} component of the ISR
       -> Double  -- ^ a guess of the longitudinal momentum of the resonance
       -> Double  -- ^ the squared mass of the resonance
       -> AT
calcAT at qx qy qz m2sq =
    case mAT at qx qy 0 of
        Nothing           -> AT sATval 0 0
        Just (mAT1, mAT2) -> AT { _sAT  = sATval
                                , _mAT1 = mAT1
                                , _mAT2 = mAT2 }
  where sATval = sAT at qx qy qz m2sq

sAT0 :: Antler
     -> FourMomentum  -- ^ four-momentum of the heavy resonance
     -> Double
sAT0 Antler {..} pRoot =
    let qp1 = pRoot `dot` _v1
        qp2 = pRoot `dot` _v2
        qSq = pRoot `dot` pRoot
        deltaM = _M1sq - _M0sq

        a00 = 2 * _M1sq
        a01 = deltaM + _mV1sq
        a02 = -deltaM + 2 * qp2 - _mV2sq
        a03 = qSq

        a10 = a01
        a11 = 2 * _mV1sq
        a12 = 2 * _v1v2
        a13 = 2 * qp1

        a20 = a02
        a21 = a12
        a22 = 2 * _mV2sq
        a23 = 2 * qp2

        a30 = a03 / qSq  -- a03
        a31 = a13 / qSq  -- a13
        a32 = a23 / qSq  -- a23
        a33 = 2          -- 2 * qSq

        m1  = Mat22 (Row2 a00 a01) (Row2 a10 a11)
        m1' = Mat22 (Row2 a22 a23) (Row2 a32 a33)

        m2  = Mat22 (Row2 a00 a02) (Row2 a10 a12)
        m2' = Mat22 (Row2 a21 a23) (Row2 a31 a33)

        m3  = Mat22 (Row2 a00 a03) (Row2 a10 a13)
        m3' = Mat22 (Row2 a21 a22) (Row2 a31 a32)

        m4  = Mat22 (Row2 a01 a02) (Row2 a11 a12)
        m4' = Mat22 (Row2 a20 a23) (Row2 a30 a33)

        m5  = Mat22 (Row2 a01 a03) (Row2 a11 a13)
        m5' = Mat22 (Row2 a20 a22) (Row2 a30 a32)

        m6  = Mat22 (Row2 a02 a03) (Row2 a12 a13)
        m6' = Mat22 (Row2 a20 a21) (Row2 a30 a31)
    in (/ (64 * _M1sq ** 3 + eps)) $
       det m1 * det m1' - det m2 * det m2'
        + det m3 * det m3' + det m4 * det m4'
        - det m5 * det m5' + det m6 * det m6'

sAT :: Antler
    -> Double  -- ^ - p_{x} component of the ISR
    -> Double  -- ^ - p_{y} component of the ISR
    -> Double  -- ^ a guess of the longitudinal momentum of the resonance
    -> Double  -- ^ the squared mass of the resonance
    -> Double
sAT at qx qy qz m2sq = sAT0 at (setXYZT qx qy qz e)
  where e = sqrt $ m2sq + qx * qx + qy * qy + qz * qz

mAT :: Antler
    -> Double  -- ^ - p_{x} component of the ISR
    -> Double  -- ^ - p_{y} component of the ISR
    -> Double  -- ^ a guess of the longitudinal momentum of the resonance
    -> Maybe (Double, Double)
mAT at@Antler{..} qx qy qz
    | _M1sq <= 0 = Nothing
    | otherwise  = do
          let f = sAT at qx qy qz
              pSq = qx * qx + qy * qy + qz * qz
              p  = sqrt pSq
              m1 = sqrt _M1sq

              f1 = f ((2 * m1 + p) ** 2 - pSq)
              f2 = f ((3 * m1 + p) ** 2 - pSq)
              f3 = f ((4 * m1 + p) ** 2 - pSq)
              denom = 2 * _M1sq

              a = (f1 - 2 * f2 + f3) / denom
              -- a = trace ("a = " ++ show a') a'
              b = (/ denom) $
                  - (2 * p +  7 * m1) * f1
                  + (4 * p + 12 * m1) * f2
                  - (2 * p +  5 * m1) * f3
              -- b = trace ("b = " ++ show b') b'
              c = (/ denom) $
                        (p + 3 * m1) * (p + 4 * m1) * f1
                  - 2 * (p + 2 * m1) * (p + 4 * m1) * f2
                  +     (p + 2 * m1) * (p + 3 * m1) * f3
              -- c = trace ("c = " ++ show c') c'

          (solET1, solET2) <- quadEqSolver a b c (1 / m1)

          let mSols = ( sqrt0 $ solET1 * solET1 - pSq
                      , sqrt0 $ solET2 * solET2 - pSq )
              sol1 = uncurry min mSols
              sol2 = uncurry max mSols
          return (sol1, sol2)



data Row2 e = Row2 !e !e deriving Show

data Mat22 e = Mat22 (Row2 e) (Row2 e) deriving Show

det :: Num e => Mat22 e -> e
det (Mat22 (Row2 a b) (Row2 c d)) = a * d - b * c

sqrt0 :: Double -> Double
sqrt0 x = if x < 0 then 1.0e+10 else sqrt x

-- | the real roots of quadratic equation: A x^2 + B x + C = 0.
--
-- If the root x0 is complex and |Im(x0)| < cut, it returns Re(x0).
quadEqSolver :: Double  -- ^ the coefficient A
             -> Double  -- ^ the coefficient B
             -> Double  -- ^ the coefficient C
             -> Double  -- ^ the cut value for complex roots
             -> Maybe (Double, Double)
quadEqSolver a b c epsScale = do
    let d = b * b - 4 * a * c
    if d >= 0
        then do let q = -0.5 * (b + signum b * sqrt d)
                return (q / (a + eps), c / (q  + eps))
        else do let r = sqrt $ c / (a + eps)
                    th = acos $ - b / (2 * sqrt (a * c) + eps)
                    x = r * cos th
                if abs (r * sin th) < epsScale
                    then return (x, x)
                    else Nothing

eps :: Double
eps = 1.0e-12
