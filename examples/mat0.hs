module Main where

import HEP.Kinematics.Antler               (getMAT)

import HEP.Kinematics                      (FourMomentum)
import HEP.Kinematics.Vector.LorentzVector (setXYZT)

main :: IO ()
main = do
    let -- ignoring the longitudinal momentum of the resonance.
        mAT0 = getMAT pV1 pV2 qx qy 0  mA mB
        -- with a guess value (qz)
        -- for the longitudinal momentum of the resonance.
        mAT' = getMAT pV1 pV2 qx qy qz mA mB
    putStrLn $ "M_{AT}(0)   = " ++ show mAT0
    putStrLn $ "M_{AT}(Q_z) = " ++ show mAT'

mA, mB :: Double
mA = 173.0
mB = 0

pV1, pV2 :: FourMomentum
pV1 = setXYZT 22.24011 26.391145 (-324.80662) 351.49241
pV2 = setXYZT 14.16262 (-48.55755) 196.35579 224.89580

qx, qy, qz :: Double
qx = 0
qy = 0
qz = -172.89418
