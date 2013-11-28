-- HERB server

-- to compile: ghc -threaded --make main.hs
-- usage: ./main [server port] +RTS -N2

module Main where

import System.Environment (getArgs)
import Network (listenOn, PortID(..), PortNumber(..), accept)
import Network.Socket.Internal (withSocketsDo)
import System.IO (hSetBuffering, BufferMode(..), Handle(..), hClose)
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Server

-- loads the HERB server 
main :: IO ()
main = withSocketsDo $ do
    -- TODO: tratar opcoes de entrada. Numero da Porta, Arquivo de Configuracao, etc.
    port <- fmap lerPorta getArgs
    socket <- listenOn $ PortNumber port 
    putStrLn $ "HERB Server Started. Listening on port " ++ show port
    forever $ do
        (handle, hostName, clntPrtNmbr) <- accept socket
        hSetBuffering handle NoBuffering -- line-buffering, block-buffering or no-buffering
        forkIO $ dispatch handle hostName >> hClose handle


lerPorta :: [String] -> PortNumber
lerPorta []  = 8080 
lerPorta [p] = fromIntegral $ read p        

