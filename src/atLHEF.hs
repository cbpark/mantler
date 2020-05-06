{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           HEP.Kinematics.Antler
import           MAT.Helper                        as MH
-- hep-utilities
import           HEP.Data.LHEF
import           HEP.Kinematics.Vector.TwoVector   (setXY)

import           Codec.Compression.GZip            (decompress)
import qualified Data.ByteString.Char8             as C
import           Data.ByteString.Lazy.Char8        (ByteString)
import qualified Data.ByteString.Lazy.Char8        as BL
import           Data.Double.Conversion.ByteString

import           Pipes
import           Pipes.ByteString                  (fromLazy)
import qualified Pipes.Prelude                     as P
--
import           Control.Monad                     (forever, unless)
import           System.Environment                (getArgs)
import           System.Exit                       (die)
import           System.IO

-- import           Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    let lenArg = length args
    unless (lenArg == 1 || lenArg == 2) $
        die "-- Usage: atLHEF <LHEF file gzipped> [output]"

    let lheFile = head args
    putStrLn $ "-- The input LHEF file is " <> lheFile <> "."
    events <- decompress <$> BL.readFile lheFile

    let writeOutput h =
            runEffect $ getLHEFEvent fromLazy events
            -- >-> P.map (calcVar 80.379 173.0 800)
            >-> P.map (calcVar 0 173.0 800)
            -- >-> P.take 100
            >-> printVar h

    if lenArg == 1
        then writeOutput stdout
        else do let outfile = args !! 1
                withFile outfile WriteMode $ \h -> do
                    BL.hPutStrLn h header
                    putStrLn "-- Calculating the event variables ..."
                    writeOutput h

                putStrLn $ "-- ... Done!\n"
                    <> "-- " <> outfile <> " has been generated."

data Var = Var { -- | Delta_{AT} for true momenta
                 _deltaATtrue :: !Double
                 -- | the AT variables for the resonance (Q = 0)
               , _AT0         :: !AT
                 -- | M_{AT} using the MAOS solutions
               , _mAT1        :: !Double
               , _mAT2        :: !Double
                 -- | M_{T2}
               , _mMAOS       :: ![Double]
               , _mTtrue      :: !Double
               , _mT2         :: !Double
               , _Qz          :: !Double
               } deriving Show

printVar :: MonadIO m => Handle -> Consumer (Maybe Var) m ()
printVar h = forever $ do
    vars <- await
    liftIO $ case vars of
                 Nothing       -> return ()  -- hPutStrLn stderr "failed!"
                 Just Var {..} -> C.hPutStrLn h $
                     toExponential 8 _deltaATtrue
                     <> "  " <> showAT _AT0
                     <> "  " <> toFixed 4 _mAT1
                     <> "  " <> toFixed 4 _mAT2
                     <> C.unwords (map (\m -> "  " <> toFixed 4 m) _mMAOS)
                     <> "  " <> toFixed 4 _mTtrue
                     <> "  " <> toFixed 4 _mT2
                     <> "  " <> toFixed 4 _Qz

calcVar :: Double -> Double -> Double -> Event -> Maybe Var
calcVar m0 m1 m2 ps = do
    (pH, pBs, ptmiss) <- selectP ps
    at <- mkAntler m0 m1 (visibles pBs)
    let (qx, qy, qz) = pxpypz pH
        at0 = calcAT at qx qy 0 m2

    return $
        case mATMAOS at qx qy ptmiss of
            Nothing -> Var { _deltaATtrue = deltaAT0 at pH
                           , _AT0         = at0
                           , _mAT1        = MH._mAT1 at0
                           , _mAT2        = MH._mAT2 at0
                           , _mMAOS       = [0, 0, 0, 0]
                           , _mTtrue      = 0
                           , _mT2         = 0
                           , _Qz          = qz }
            Just (mAT1, mAT2, mMAOS, mTtrue, mT2) ->
                Var { _deltaATtrue = deltaAT0 at pH
                    , _AT0         = at0
                    , _mAT1        = mAT1
                    , _mAT2        = mAT2
                    , _mMAOS       = mMAOS
                    , _mTtrue      = mTtrue
                    , _mT2         = mT2
                    , _Qz          = qz }

selectP :: Event -> Maybe (FourMomentum, [FourMomentum], TransverseMomentum)
selectP ev = do
    let topChild = particlesFrom topQuarks (eventEntry ev)
    if null topChild
        then Nothing
        else do let pH = momentumSum $ fourMomentum <$> concat topChild
                    pV = momentumSum . fmap fourMomentum <$>
                        (filter (not . isNeutrino) <$> topChild)
                    pNu = momentumSum (momentumSum . fmap fourMomentum <$>
                          (filter isNeutrino <$> topChild))
                    ptmiss = setXY (px pNu) (py pNu)
                return (pH, pV, ptmiss)
  where
    topQuarks = ParticleType [6]
    isNeutrino = (`elem` neutrinos) . idOf

{-
selectP :: Event -> Maybe (FourMomentum, [FourMomentum], TransverseMomentum)
selectP ev = do
    let [topChild, wChild] = flip particlesFrom (eventEntry ev) <$>
                             [topQuarks, wBosons]
    if null topChild
        then Nothing
        else do let pH  = momentumSum $ fourMomentum <$> concat topChild
                    pBs = fourMomentum <$> concat (filter isBquark <$> topChild)
                    pWW = momentumSum $ fourMomentum <$> concat wChild
                    ptmiss = setXY (px pWW) (py pWW)
                return (pH, pBs, ptmiss)
  where
    topQuarks = ParticleType [6]
    wBosons   = ParticleType [24]
    isBquark = (== 5) . abs . idOf
-}

header :: ByteString
header = BL.pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
             [ "deltaATtrue"
             , "deltaAT(0)", "mATmin(0)", "mATmax(0)"
             , "mATmin(maos)", "mATmax(maos)"
             , "mMAOS1", "mMAOS2", "mMAOS3", "mMAOS4"
             , "mTtrue", "mT2", "Qz" ])
