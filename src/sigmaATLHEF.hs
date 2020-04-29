{-# LANGUAGE RecordWildCards #-}

module Main where

import           HEP.Kinematics.Antler

import           HEP.Data.LHEF

import           Codec.Compression.GZip     (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import           Pipes
import           Pipes.ByteString           (fromLazy)
import qualified Pipes.Prelude              as P

import           System.Environment         (getArgs)
import Control.Monad (forever)

main :: IO ()
main = do
    lheFile <- head <$> getArgs
    events <- decompress <$> B.readFile lheFile
    runEffect $ getLHEFEvent fromLazy events
        >-> P.map (calcAT 80.379 173.0 800)
        -- >-> P.map (calcAT 0 173.0 800)
        >-> P.take 50
        >-> printAT

data AT = AT { _sAT  :: !Double
             , _sAT0 :: !Double
             , _mAT1 :: !Double
             , _mAT2 :: !Double
             } deriving Show

printAT :: MonadIO m => Consumer (Maybe AT) m ()
printAT = forever $ do
    vars <- await
    case vars of
        Nothing      -> liftIO . putStrLn $ "nothing!"  -- return ()
        Just AT {..} -> liftIO . putStrLn $
            show _sAT <> "\t" <> show _sAT0 <> "\t"
            <> show _mAT1 <> "\t" <> show _mAT2

calcAT :: Double -> Double -> Double -> Event -> Maybe AT
calcAT m0 m1 m2 ps = do
    (pH, pBs) <- selectP ps
    at <- mkAntler m0 m1 (visibles pBs)
    (mAT1, mAT2) <- mAT0 at
    return $ AT { _sAT  = sAT  at pH
                , _sAT0 = sAT0 at m2
                , _mAT1 = mAT1
                , _mAT2 = mAT2 }

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
