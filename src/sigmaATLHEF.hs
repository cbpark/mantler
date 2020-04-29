module Main where

import           HEP.Kinematics.Antler

import           HEP.Data.LHEF

import           Codec.Compression.GZip     (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import           Pipes
import           Pipes.ByteString           (fromLazy)
import qualified Pipes.Prelude              as P

import           System.Environment         (getArgs)

main :: IO ()
main = do
    lheFile <- head <$> getArgs
    events <- decompress <$> B.readFile lheFile
    runEffect $ getLHEFEvent fromLazy events
        >-> P.map (getSAT 80.379 173.0)
        -- >-> P.map (getSAT' 0 173.0)
        >-> P.print

pH' :: FourMomentum
pH' = setXYZT 0 0 0 (1 * 800)

getSAT :: Double -> Double -> Event -> Maybe (Double, Double)
getSAT m0 m1 ps = do
    (pH, pBs) <- selectP ps
    at <- mkAntler m0 m1 (visibles pBs)
    return (sAT at pH, sAT at pH')

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

getSAT' :: Double -> Double -> Event -> Maybe (Double, Double)
getSAT' m0 m1 ps = do
    (pH, pBs) <- selectP' ps
    at <- mkAntler m0 m1 (visibles pBs)
    return (sAT at pH, sAT at pH')

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
