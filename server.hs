module Server where
-- HERB server's functions 

import System.IO (Handle(..), hPutStr, hClose, hGetContents)
import Data.Time

import DataDef

dispatch :: Handle -> String -> IO ()
dispatch handle hostname = do  
    putStrLn $ "*** Request from " ++ hostname ++ "***"
    rqst <- fmap lines (hGetContents handle) --[String]
    let headRqst = head rqst
    putStrLn $ headRqst ++ "\n"
    let parsedRqst = parseRequest $ headRqst
    putStrLn $ "*** REQUEST ***\n" ++ (show parsedRqst) ++ "\n"
    let response = buildResponse parsedRqst
    putStrLn $ "*** RESPONSE ***\n" ++ (show response) ++ "\n"
    reply handle response
    return ()

parseRequest :: String -> Request
parseRequest lista = case (head (words (lista))) of
    "GET" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = (words lista) !! 1, rMethod = GET, rParams = []}
    "POST" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = (words lista) !! 1, rMethod = POST, rParams = []}
    "PUT" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = (words lista) !! 1, rMethod = PUT, rParams = []}
    "DELETE" -> Request RequestHeaders { reqConType = "application/json", rAccept = "*/*"} RequestBody { rURI = (words lista) !! 1, rMethod = DELETE, rParams = []}
    _  ->  Request RequestHeaders { reqConType = "", rAccept = ""}  RequestBody { rURI = (words lista) !! 1, rMethod = UNDEFINED, rParams = [] }

buildResponse :: Request -> Response
buildResponse (Request hd bd)
    | rMethod(bd) == GET = Response ResponseHeaders { statusCode = "200", resDate = "", resConType = "application/json" } "{ response for GET }"
    | rMethod(bd) == POST = Response ResponseHeaders { statusCode = "200", resDate = "", resConType = "application/json" } "{ response for POST }"
    | rMethod(bd) == PUT = Response ResponseHeaders { statusCode = "200", resDate = "", resConType = "application/json" } "{ response for PUT }"
    | rMethod(bd) == DELETE = Response ResponseHeaders { statusCode = "200", resDate = "", resConType = "application/json" } "{ response for DELETE }"
    | rMethod(bd) == UNDEFINED = Response ResponseHeaders { statusCode = "405", resDate = "", resConType = "application/json" } "{ UNDEFINED METHOD }"

reply :: Handle -> Response -> IO ()
reply handle response@(Response hdr bdy) = do
    cT <- fmap show getCurrentTime  
    let respHeader = hdr { resDate = cT }
    let rsp = Response respHeader bdy
    hPutStr handle $ show rsp
    hClose handle
    return ()
    