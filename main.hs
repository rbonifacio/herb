-- HERB server 0.1

-- to compile: ghc -threaded --make main.hs
-- usage: ./main [server port] +RTS -N2

module Main where

import System.Environment (getArgs)
import Network (listenOn, PortID(..), accept)
import System.IO (hSetBuffering, BufferMode(..), Handle(..))
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Server

-- loads the HERB server 
main :: IO ()
main = do
    args <- getArgs
    --; if (length args > 0) then 
    let port = fromIntegral (read $ head args :: Int)
        --else 
        --    let port = 8080            
    socket <- listenOn $ PortNumber port
    putStrLn $ "HERB Server Started. Listening on port " ++ show port
    forever $ do
        (handle, hostName, portNumber) <- accept socket
        -- hSetBuffering handle NoBuffering
        forkIO $ dispatch handle hostName