{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MAT.Helper where

import HEP.Kinematics.Antler             (Antler, deltaAT, mAT)

import Data.ByteString                   (ByteString)
import Data.Double.Conversion.ByteString (toExponential, toFixed)

data AT = AT { _deltaAT :: !Double  -- ^ Delta_{AT}
             , _mAT1    :: !Double  -- ^ min(M_{AT})
             , _mAT2    :: !Double  -- ^ max(M_{AT})
             } deriving Show

showAT :: AT -> ByteString
showAT AT {..} =
    toExponential 8 _deltaAT <> "  " <> toFixed 4 _mAT1 <> "  " <> toFixed 4 _mAT2

calcAT :: Antler
       -> Double  -- ^ - p_{x} component of the ISR
       -> Double  -- ^ - p_{y} component of the ISR
       -> Double  -- ^ a guess of the longitudinal momentum of the resonance
       -> Double  -- ^ the energy of the resonance
       -> AT
calcAT at qx qy qz e =
    case mAT at qx qy 0 of
        Nothing           -> AT deltaATval 0 0
        Just (mAT1, mAT2) -> AT { _deltaAT  = deltaATval
                                , _mAT1 = mAT1
                                , _mAT2 = mAT2 }
  where deltaATval = deltaAT at qx qy qz e
