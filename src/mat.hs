module Main where

import           HEP.Data.LHEF              (getLHEFEvent)

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
    runEffect $ getLHEFEvent fromLazy events >-> P.take 3 >-> P.print
