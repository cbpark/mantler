module Main where

import           HEP.Kinematics.Antler

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil    as U
-- import           HEP.Kinematics             (FourMomentum)

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
    -- B.putStrLn events
    -- runEffect $ getLHEFEvent fromLazy events >-> P.take 3 >-> P.print
    runEffect $ getLHEFEvent fromLazy events
        >-> P.map snd >-> U.finalStates >-> getBquarks
        >-> visibles >-> P.map (mkAntler 80.379 173.0)
        >-> P.print

getBquarks :: Monad m => Pipe [Particle] [Particle] m ()
getBquarks = P.map $ filter ((== 5) . abs . idOf)
