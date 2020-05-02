{-# LANGUAGE RecordWildCards #-}

module HEP.Kinematics.Antler where

import HEP.Util

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

deltaAT :: Antler
        -> Double  -- ^ - p_{x} component of the ISR
        -> Double  -- ^ - p_{y} component of the ISR
        -> Double  -- ^ a guess of the longitudinal momentum of the resonance
        -> Double  -- ^ the energy of the resonance
        -> Double
deltaAT at qx qy qz e = deltaAT0 at (setXYZT qx qy qz e)

deltaAT0 :: Antler
         -> FourMomentum  -- ^ four-momentum of the heavy resonance
         -> Double
deltaAT0 Antler {..} pRoot =
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

        a30 = a03
        a31 = a13
        a32 = a23
        a33 = 2 * qSq

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
    in (/ (_M1sq ** 4 + 1.0e-12)) $
       det m1 * det m1' - det m2 * det m2'
        + det m3 * det m3' + det m4 * det m4'
        - det m5 * det m5' + det m6 * det m6'

mAT :: Antler
    -> Double  -- ^ - p_{x} component of the ISR
    -> Double  -- ^ - p_{y} component of the ISR
    -> Double  -- ^ a guess of the longitudinal momentum of the resonance
    -> Maybe (Double, Double)
mAT at@Antler{..} qx qy qz
    | _M1sq <= 0 = Nothing
    | otherwise  = do
          let f = deltaAT at qx qy qz
              m1 = sqrt _M1sq

          (_, sol) <- quarticEqSol f [m1, 10 * m1, 100 * m1, 1000 * m1] 1.0e-3
          -- let !nsol' = trace ("nsol = " ++ show nsol) nsol

          if null sol
              then Nothing
              else do let (eT1, eT2) = (,) <$> minimum <*> maximum $ sol
                          pSq = qx * qx + qy * qy + qz * qz
                      return ( sqrt0 $ eT1 * eT1 - pSq
                             , sqrt0 $ eT2 * eT2 - pSq )
    where
      sqrt0 x = if x < 0 then 1.0e+10 else sqrt x
