module HEP.Kinematics.Antler where

import           HEP.Kinematics

import           Pipes          (Pipe)
import qualified Pipes.Prelude  as P

data Antler = Antler { _M0sq  :: !Double
                     , _M1sq  :: !Double
                     , _mV1sq :: !Double
                     , _mV2sq :: !Double
                     , _v1    :: !FourMomentum
                     , _v2    :: !FourMomentum
                     , _v1v2  :: !Double
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
                                             , _v1v2  = p1 `dot` p2
                                             }

data Visibles = NE
              | Visibles
                { _p1 :: FourMomentum
                , _p2 :: FourMomentum }
              deriving Show

visibles :: (Monad m, HasFourMomentum p) => Pipe [p] Visibles m ()
visibles = P.map visibles'
    where
      visibles' ps | length ps == 2 = let [p1, p2] = ps
                                      in Visibles { _p1 = fourMomentum p1
                                                  , _p2 = fourMomentum p2 }
                   | otherwise      = NE
