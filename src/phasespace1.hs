{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           HEP.Kinematics.Antler
import           MAT.Helper

import           HEP.Data.LHEF

import           Codec.Compression.GZip            (decompress)
import qualified Data.ByteString.Char8             as C
import           Data.ByteString.Lazy.Char8        (ByteString)
import qualified Data.ByteString.Lazy.Char8        as BL
import           Data.Double.Conversion.ByteString (toExponential)

import           Pipes
import           Pipes.ByteString                  (fromLazy)
import qualified Pipes.Prelude                     as P

import           Control.Monad                     (forever, unless)
import           System.Environment                (getArgs)
import           System.Exit                       (die)
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 2) $
        die "-- Usage: phasespace <LHEF file gzipped> <output>"

    let lheFile = head args
    putStrLn $ "-- The input LHEF file is " <> lheFile <> "."
    events <- decompress <$> BL.readFile lheFile

    let outfile = args !! 1
    withFile outfile WriteMode $ \h -> do
        BL.hPutStrLn h header
        putStrLn "-- Calculating the event variables ..."
        runEffect $ getLHEFEvent fromLazy events
            >-> P.map (calcVar 80.379 173.0 800)
            -- >-> P.take 3
            >-> printVar h

    putStrLn $ "-- ... Done!\n" <> "-- " <> outfile <> " has been generated."

data Var = Var { _deltaAT1 :: !Double -- ^ the AT variable for M2 = M2(true) / 2
               , _deltaAT2 :: !Double -- ^ the AT variable for M2 = 2 * M2(true)
               , _atTrue   :: !AT  -- ^ the AT variabless for M2 = M2(true)
               } deriving Show

calcVar :: Double -> Double -> Double -> Event -> Maybe Var
calcVar m0 m1 m2 ps = do
    (_, pBs) <- selectP ps
    at <- mkAntler m0 m1 (visibles pBs)
    let deltaATval1 = deltaAT at 0 0 0 (m2 / 2)
        deltaATval2 = deltaAT at 0 0 0 (2 * m2)
        atTrue = calcAT at 0 0 0 m2
    return $ Var { _deltaAT1 = deltaATval1
                 , _deltaAT2 = deltaATval2
                 , _atTrue   = atTrue }

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

printVar :: MonadIO m => Handle -> Consumer (Maybe Var) m ()
printVar h = forever $ do
    vars <- await
    liftIO $
        case vars of
            Nothing       -> hPutStrLn stderr "failed!"
            Just Var {..} -> C.hPutStrLn h $
                toExponential 8 _deltaAT1 <> "  " <> toExponential 8 _deltaAT2
                <> "  " <> showAT _atTrue

header :: ByteString
header = BL.pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
             [ "deltaAT(M2/2)", "deltaAT(2*M2)"
             , "deltaAT(true)", "mAT(min)", "mAT(max)" ])
