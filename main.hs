-- HERB server 0.1

-- to compile: ghc -threaded --make main.hs
-- usage: ./main [server port] +RTS -N2

module Main where

import System.Environment (getArgs)
import Network (listenOn, PortID(..), PortNumber(..), accept)
import System.IO (hSetBuffering, BufferMode(..), Handle(..))
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Server

-- loads the HERB server 
main :: IO ()
main = do
    port <- fmap lerPorta getArgs
    socket <- listenOn $ PortNumber port 
    putStrLn $ "HERB Server Started. Listening on port " ++ show port
    forever $ do
        (handle, hostName, clientPortNumber) <- accept socket
        hSetBuffering handle NoBuffering
        forkIO $ dispatch handle hostName

lerPorta :: [String] -> PortNumber
lerPorta []  = 8080 
lerPorta [p] = fromIntegral $ read p        
