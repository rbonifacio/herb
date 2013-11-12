module Server where
-- HERB server's functions 

import System.IO (Handle(..), hPutStr)

import DataDef

dispatch :: Handle -> String -> IO ()
dispatch handle hostname = do  
    putStrLn $ "Request from " ++ hostname
    reply handle

reply :: Handle -> IO ()
reply handle = do
    let resp = Response ResponseHeaders {statusCode = "200", resDate = "Data/Hora", resConType = "application/json" } "{ JSON BODY }"
    hPutStr handle $ show resp
