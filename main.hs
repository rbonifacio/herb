-- HERB server 0.1

-- to compile: ghc --make main.hs
-- usage: ./main [server port]

module Main where

import System.Environment (getArgs)
import Network (listenOn, PortID(..), accept)
import System.IO (hSetBuffering, BufferMode(..), hClose, Handle(..))
import Control.Monad (forever)

import Server

-- loads the HERB server 
main :: IO ()
main = do
    args <- getArgs
    let porta = fromIntegral (read $ head args :: Int)
    socket <- listenOn $ PortNumber porta
    putStrLn $ "HERB Server Started. Listening on port " ++ show porta
    forever $ do
        (handle, hostName, portNumber) <- accept socket
        dispatch handle hostName
        hClose handle
