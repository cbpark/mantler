{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

-- import           MAT.Combinatorics                 (correctPairs)
-- import           MAT.Helper                        as MH

-- import           HEP.Kinematics.Antler
-- hep-utilities
import           HEP.Data.LHCO
-- import           HEP.Kinematics.Vector.TwoVector   (setXY)

import           Codec.Compression.GZip     (decompress)
-- import qualified Data.ByteString.Char8             as C
-- import           Data.ByteString.Lazy.Char8        (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
-- import           Data.Double.Conversion.ByteString

import           Pipes
import           Pipes.ByteString           (fromLazy)
import qualified Pipes.Prelude              as P
--
import           Control.Monad              (forever, unless)
import           Data.List                  (sortBy)
import           System.Environment         (getArgs)
import           System.Exit                (die)
import           System.IO

-- import           Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    let lenArg = length args
    unless (lenArg == 1 || lenArg == 2) $
        die "-- Usage: atLHCO <LHCO file gzipped> [output]"

    let lhcFile = head args
    putStrLn $ "-- The input LHCO file is " <> lhcFile <> "."
    events <- decompress <$> BL.readFile lhcFile

    let writeOutput h =
            runEffect $ getLHCOEvent fromLazy events
            >-> P.take 20
            >-> basicSelection
            >-> takeDLEvent
            -- >-> P.map (calcVar 80.379 173.0 800)
            -- >-> P.map (calcVar 0 173.0 800)
            -- >-> printVar h
            >-> P.print

    if lenArg == 1
        then writeOutput stdout
        else do let outfile = args !! 1
                withFile outfile WriteMode $ \h -> do
                    -- BL.hPutStrLn h header
                    putStrLn "-- Calculating the event variables ..."
                    writeOutput h

                putStrLn $ "-- ... Done!\n"
                    <> "-- " <> outfile <> " has been generated."

basicSelection :: Monad m => Pipe Event Event m ()
basicSelection = forever $ do
  Event {..} <- await
  let evFiltered = Event { nev      = nev
                         , photon   = filter basicSelection' photon
                         , electron = filter basicSelection' electron
                         , muon     = filter basicSelection' muon
                         , tau      = filter basicSelection' tau
                         , jet      = filter basicSelection' jet
                         , bjet     = filter basicSelection' bjet
                         , met      = met }
  yield evFiltered

basicSelection' :: PhyObj a -> Bool
basicSelection' ObjPhoton {}                           = False
basicSelection' (ObjElectron (Track (eta', _, pt')) _) = abs eta' < 2.4 && pt' > 20
basicSelection' (ObjMuon (Track (eta', _, pt')) _ _)   = abs eta' < 2.4 && pt' > 20
basicSelection' ObjTau {}                              = False
basicSelection' ObjJet {}                              = False
basicSelection' (ObjBjet (Track (eta', _, pt')) _ _ _) = abs eta' < 2.4 && pt' > 20
basicSelection' (ObjMet (_, pt'))                      = pt' > 40
basicSelection' ObjUnknown                             = False

data DLEvent = DLEvent { leptons :: [FourMomentum]
                       , jets    :: [FourMomentum]
                       , missing :: TransverseMomentum
                       } deriving Show

takeDLEvent :: Monad m => Pipe Event (Maybe DLEvent) m ()
takeDLEvent = forever $ do
    ev@Event {..} <- await
    let electrons = fourMomentum <$> electron
        muons     = fourMomentum <$> muon
        leptons'  = sortBy ptCompare (electrons <> muons)
        jets'     = fourMomentum <$> bjet
    yield $ if length leptons' < 2 || length jets' < 2
            then Nothing
            else Just $ DLEvent { leptons = leptons'
                                , jets    = jets'
                                , missing = missingET ev }


-- data Var = Var { _AT0         :: !AT
--                , _mAT1        :: !Double
--                , _mAT2        :: !Double
--                , _mMAOS       :: ![Double]
--                , _mTtrue      :: !Double
--                , _mT2         :: !Double
--                , _correctPair :: !Int
--                } deriving Show

-- printVar :: MonadIO m => Handle -> Consumer (Maybe Var) m ()
-- printVar h = forever $ do
--     vars <- await
--     liftIO $ case vars of
--                  Nothing       -> return ()
--                  Just Var {..} -> C.hPutStrLn h $
--                      showAT _AT0
--                      <> "  " <> toFixed 4 _mAT1
--                      <> "  " <> toFixed 4 _mAT2
--                      <> C.unwords (map (\m -> "  " <> toFixed 4 m) _mMAOS)
--                      <> "  " <> toFixed 4 _mTtrue
--                      <> "  " <> toFixed 4 _mT2
--                      <> "  " <> C.pack (show _correctPair)

-- calcVar :: Double -> Double -> Double -> Event -> Maybe Var
-- calcVar m0 m1 m2 ps = do
--     (pH, pVs, ptmiss, pairp) <- selectP ps
--     at <- mkAntler m0 m1 (visibles pVs)
--     let (qx, qy) = pxpy pH
--         correctPair = if pairp then 1 else -1
--         at0 = calcAT at qx qy 0 m2

--     return $
--         case mATMAOS at qx qy ptmiss of
--             Nothing -> Var { _AT0         = at0
--                            , _mAT1        = MH._mAT1 at0
--                            , _mAT2        = MH._mAT2 at0
--                            , _mMAOS       = [0, 0, 0, 0]
--                            , _mTtrue      = mTtrue at ptmiss
--                            , _mT2         = 0
--                            , _correctPair = correctPair }
--             Just (mAT1, mAT2, mMAOS, mT2) ->
--                 Var { _AT0         = at0
--                     , _mAT1        = mAT1
--                     , _mAT2        = mAT2
--                     , _mMAOS       = mMAOS
--                     , _mTtrue      = mTtrue at ptmiss
--                     , _mT2         = mT2
--                     , _correctPair = correctPair }

-- selectP :: Event
--         -> Maybe (FourMomentum, [FourMomentum], TransverseMomentum, Bool)
-- selectP ev = do
--     let topChild = particlesFrom topQuarks (eventEntry ev)
--     if null topChild
--         then Nothing
--         else do let pH = momentumSum $ fourMomentum <$> concat topChild
--                     pBLs = fmap fourMomentum <$>
--                           (filter (not . isNeutrino) <$> topChild)
--                     pNu = momentumSum (momentumSum . fmap fourMomentum <$>
--                                        (filter isNeutrino <$> topChild))
--                     ptmiss = setXY (px pNu) (py pNu)

--                 pBLpairs <- correctPairs pBLs ptmiss 173.0 80.379 0 153.173

--                 let pVs = momentumSum <$> pBLpairs
--                 return (pH, pVs, ptmiss, pBLs == pBLpairs)
--   where
--     topQuarks = ParticleType [6]
--     isNeutrino = (`elem` neutrinos) . idOf

-- header :: ByteString
-- header = BL.pack $ "# " <>
--          foldl1 (\v1 v2 -> v1 <> ", " <> v2)
--          (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
--              [ "deltaAT(0)", "mATmin(0)", "mATmax(0)"
--              , "mATmin(maos)", "mATmax(maos)"
--              , "mMAOS1", "mMAOS2", "mMAOS3", "mMAOS4"
--              , "mTtrue", "mT2", "correct pair" ])