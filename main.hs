-- HERB server 0.1

-- to compile: ghc --make main.hs
-- usage: ./main [server port]

module Main where

import System.Environment (getArgs)
import Network (listenOn, PortID(..), accept)
import System.IO (hSetBuffering, BufferMode(..), hClose, hPutStr, Handle(..))
import Control.Monad (forever)

data RequestMethod = GET 
                   | POST 
                   | PUT
                   | DELETE
                   deriving (Show)

data RequestHeaders = RequestHeaders {
                    reqConType :: String,    -- Request Content-type, the mime type of the request
                    rAccept :: String   -- */*
}

data RequestBody = RequestBody {
                    rURI :: String,             -- Request Universal Resourse Identifier
                    rMethod :: RequestMethod,   -- Request Type
                    rParams :: [(String,String)]--  Parâmetros (chave,valor)
}    

data Request = Request RequestHeaders RequestBody

data ResponseHeaders = ResponseHeaders {
                    statusCode :: String,
                    resDate :: String,
                    resConType :: String         -- Response Content-type, the mime type of this content 
}
type ResponseBody = String

data Response = Response ResponseHeaders ResponseBody -- deriving(Show)

instance Show ResponseHeaders where
    show resp = "Status Code: " ++ statusCode(resp) ++ "\nDate: " ++ resDate(resp) ++ "\nContent-Type:" ++ resConType(resp) ++ "\n"

instance Show Response where
    show (Response headers body) = show(headers) ++ body ++ "\n"

main :: IO ()
main = do
    args <- getArgs
    let porta = fromIntegral (read $ head args :: Int)
    socket <- listenOn $ PortNumber porta
    putStrLn $ "HERB Server Started. Listening on port " ++ show porta
    forever $ do
        (handle, hostName, portNumber) <- accept socket
        despachar handle hostName
        hClose handle

despachar :: Handle -> String -> IO ()
despachar handle hostname = do  
    putStrLn $ "Request from " ++ hostname
    responder handle

responder :: Handle -> IO ()
responder handle = do
    let resposta = Response ResponseHeaders {statusCode = "200", resDate = "Data/Hora", resConType = "application/json" } "{ JSON BODY }"
    hPutStr handle $ show resposta
