{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           HEP.Kinematics.Antler

import           HEP.Data.LHEF

import           Codec.Compression.GZip            (decompress)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as C
import qualified Data.ByteString.Lazy.Char8        as BL
import           Data.Double.Conversion.ByteString
import           Pipes
import           Pipes.ByteString                  (fromLazy)
import qualified Pipes.Prelude                     as P

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
        die "-- Usage: mATLHEF <LHEF file gzipped> [output]"

    let lheFile = head args
    events <- decompress <$> BL.readFile lheFile

    let writeOutput h =
            runEffect $ getLHEFEvent fromLazy events
            >-> P.map (calcVar 80.379 173.0 800)
            -- >-> P.map (calcVar 0 173.0 800)
            >-> P.take 3
            >-> printVar h

    if lenArg == 1
        then writeOutput stdout
        else do let outfile = args !! 1
                withFile outfile WriteMode $ \h -> do
                    BL.hPutStrLn h header
                    writeOutput h

showAT :: AT -> ByteString
showAT AT {..} =
    toExponential 8 _sAT <> "  " <> toFixed 4 _mAT1 <> "  " <> toFixed 4 _mAT2

data Var = Var { -- | sigma_{AT} for true momenta
                 _sATtrue :: !Double
                 -- | the AT variables for the resonance at rest
               , _AT0     :: !AT
                 -- | the AT variables for the resonance with known p_T
               , _AT      :: !AT
                 -- | the longitudinal momentum of the resonance
               , _Qz      :: !Double
               } deriving Show

printVar :: MonadIO m => Handle -> Consumer (Maybe Var) m ()
printVar h = forever $ do
    vars <- await
    liftIO $ case vars of
                 Nothing       -> hPutStrLn stderr "failed!"  -- return ()
                 Just Var {..} -> C.hPutStrLn h $
                     toExponential 8 _sATtrue
                     <> "  " <> showAT _AT0
                     <> "  " <> showAT _AT
                     <> "  " <> toFixed 4 _Qz

calcVar :: Double -> Double -> Double -> Event -> Maybe Var
calcVar m0 m1 m2 ps = do
    (pH, pBs) <- selectP ps
    at <- mkAntler m0 m1 (visibles pBs)
    let qx = px pH
        qy = py pH
        qz = pz pH
        m2sq = m2 * m2

        -- assuming that the resonance was produced at rest
        at0 = calcAT at 0 0 0 m2sq

        -- assuming that the transverse momenta
        -- of the resonance are known a priori
        atT = calcAT at qx qy 0 m2sq

    return $ Var { _sATtrue = sAT0 at pH
                 , _AT0     = at0
                 , _AT      = atT
                 , _Qz      = qz }

selectP :: Event -> Maybe (FourMomentum, [FourMomentum])
selectP ev = do
    let topChild = particlesFrom topQuarks (eventEntry ev)
    if null topChild
        then Nothing
        else do let pH = momentumSum $ fourMomentum <$> concat topChild
                    pB = fourMomentum <$> concat (filter isBquark <$> topChild)
                return (pH, pB)
  where
    topQuarks = ParticleType [6]
    isBquark = (== 5) . abs . idOf

{-
selectP' :: Event -> Maybe (FourMomentum, [FourMomentum])
selectP' ev = do
    let topChild = particlesFrom topQuarks (eventEntry ev)
    if null topChild
        then Nothing
        else do let pH = momentumSum $ fourMomentum <$> concat topChild
                    pV = momentumSum . fmap fourMomentum <$>
                        (filter (not . isNeutrino) <$> topChild)
                return (pH, pV)
  where
    topQuarks = ParticleType [6]
    isNeutrino = (`elem` neutrinos) . idOf
-}

header :: BL.ByteString
header = BL.pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
             [ "sATtrue"
             , "sAT(0)", "mATmin(0)", "mATmax(0)"
             , "sAT(QT)", "mATmin(QT)", "mATmax(QT)"
             , "Qz" ])
