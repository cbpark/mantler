{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           HEP.Kinematics.Antler
import           MAT.Helper

import           HEP.Data.LHEF

import           Codec.Compression.GZip     (decompress)
import qualified Data.ByteString.Char8      as C
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import           Pipes
import           Pipes.ByteString           (fromLazy)
import qualified Pipes.Prelude              as P

import           Control.Monad              (forever, unless)
import           System.Environment         (getArgs)
import           System.Exit                (die)
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 2) $
        die "-- Usage: phasespace2 <LHEF file gzipped> <output>"

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

data Var = Var { _at0 :: !AT  -- ^ the AT variables for p_T = 0
               , _at1 :: !AT  -- ^ the AT variables for p_T = p_T^{true}
               } deriving Show

calcVar :: Double -> Double -> Double -> Event -> Maybe Var
calcVar m0 m1 m2 ps = do
    (pH, pBs) <- selectP ps
    at <- mkAntler m0 m1 (visibles pBs)
    let qx = px pH
        qy = py pH
        at0 = calcAT at 0  0  0 m2
        at1 = calcAT at qx qy 0 (sqrt $ m2 * m2 + qx * qx + qy * qy)
    return $ Var at0 at1

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
            Just Var {..} -> C.hPutStrLn h $ showAT _at0 <> "  " <> showAT _at1

header :: ByteString
header = BL.pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
             [ "deltaAT(0)",  "mAT(min, 0)",  "mAT(max, 0)"
             , "deltaAT(pT)", "mAT(min, pT)", "mAT(max, pT)" ])
