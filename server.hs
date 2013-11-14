module Server where
-- HERB server's functions 

import System.IO (Handle(..), hPutStr, hClose, hGetContents)
import Data.Time

import DataDef

dispatch :: Handle -> String -> IO ()
dispatch handle hostname = do  
    putStrLn $ "Request from " ++ hostname
    request <- fmap lines (hGetContents handle) --[String]
    putStrLn $ head request
    let req = parseRequest request
    let response = buildResponse req
    reply handle response
    return ()

parseRequest :: [String] -> Request
parseRequest lista = case (head (words (head lista))) of
    "GET" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = "temp", rMethod = GET, rParams = []}
    "POST" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = "temp", rMethod = POST, rParams = []}
    "PUT" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = "temp", rMethod = PUT, rParams = []}
    "DELETE" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = "temp", rMethod = DELETE, rParams = []}

buildResponse :: Request -> Response
buildResponse (Request hd bd)
    | rMethod(bd) == GET = Response ResponseHeaders { statusCode = "200", resDate = dt, resConType = "application/json" } "{ response for GET }"
    | rMethod(bd) == POST = Response ResponseHeaders { statusCode = "200", resDate = dt, resConType = "application/json" } "{ response for POST }"
    | rMethod(bd) == PUT = Response ResponseHeaders { statusCode = "200", resDate = dt, resConType = "application/json" } "{ response for PUT }"
    | rMethod(bd) == DELETE = Response ResponseHeaders { statusCode = "200", resDate = dt, resConType = "application/json" } "{ response for DELETE }"
    where dt = getTime

getTime :: String
getTime =
    --do { 
    --; cT <- fmap show getCurrentTime
    --; return cT
    --}
    "2013 - 11 - .."  

reply :: Handle -> Response -> IO ()
reply handle response = do
    hPutStr handle $ show response
    hClose handle
    return ()
    