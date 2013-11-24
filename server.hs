module Server where
-- HERB server's functions 

import System.IO (Handle(..), hPutStr, hClose, hGetContents)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time

import DataDef

dispatch :: Handle -> String -> IO ()
dispatch handle hostname = do  
    putStrLn $ "*** Request from " ++ hostname ++ "***"
    -- Read Request from Client
    rqst <- fmap lines (hGetContents handle) --[String]
    let headRqst = head rqst
    putStrLn $ headRqst ++ "\n"
    -- Parse Request
    let parsedRqst = parseRequest $ headRqst
    putStrLn $ "*** REQUEST ***\n" ++ (show parsedRqst) ++ "\n"
    -- Generate Response
    let response = buildResponse parsedRqst
    putStrLn $ "*** RESPONSE ***\n" ++ (show response) ++ "\n"
    -- Send Response Back to client
    reply handle response
    return ()

parseRequest :: String -> Request
parseRequest lista = 
    let uri =  (words lista) !! 1
        version = "HTTP/1.1"
        contype = "application/json"
        acpt = "*/*"
        rqst :: RequestMethod -> Request
        rqst m = Request 
                 Rqst { rURI = uri, rMethod = m, rVersion = version } 
                 RequestHeaders { reqConType = contype, rAccept = acpt } 
                 RequestBody { rParams = [] }
    in case (head (words (lista))) of
        "GET" -> rqst GET
        "POST" -> rqst POST
        "PUT" -> rqst PUT
        "DELETE" -> rqst DELETE
        _ -> rqst UNDEFINED

buildResponse :: Request -> Response
buildResponse (Request rqst hdr bdy) = 
    let date = unsafePerformIO $ fmap show getCurrentTime
        server = "HERB Server 0.1"
        contype = "application/json"
    in case (rMethod(rqst)) of 
        GET -> Response
               ResponseHeaders { statusCode = "200", resDate = date, resConType = contype, resServer = server}
               "{ response for GET }"
        POST -> Response
               ResponseHeaders { statusCode = "200", resDate = date, resConType = contype, resServer = server}
               "{ response for POST }"
        PUT -> Response
               ResponseHeaders { statusCode = "200", resDate = date, resConType = contype, resServer = server}
               "{ response for PUT }"
        DELETE -> Response
               ResponseHeaders { statusCode = "200", resDate = date, resConType = contype, resServer = server}
               "{ response for DELETE }"
        UNDEFINED -> Response
               ResponseHeaders { statusCode = "405", resDate = date, resConType = contype, resServer = server}
               "{ response for GET }"

reply :: Handle -> Response -> IO ()
reply handle rsp = do
    hPutStr handle $ show rsp
    hClose handle
    return ()
