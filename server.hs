{-# LANGUAGE DoAndIfThenElse #-} 

module Server where
-- HERB server's functions 

import System.IO (Handle(..), hPutStr, hClose, hGetLine, hGetChar)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time (getCurrentTime)
import Data.Maybe (fromJust)

import DataDef

version :: String
version = "HERB Server 0.1"

dispatch :: Handle -> String -> IO ()
dispatch handle hostname = do  
    putStrLn $ (unsafePerformIO $ fmap show getCurrentTime) ++ " : Request from " ++ hostname
    let getLines = repeat (hGetLine handle) -- [IO String]
    lns <- takeLines getLines               -- [String]
    let parsedRqst@(Request r h b) = parseRequest lns
    bdy <- getBody handle (getContLen parsedRqst) (return [])  
    let updatedRqst = (Request r h bdy)
    -- (verbose) putStrLn $ "*** REQUEST ***\n" ++ (show updatedRqst)
    let response = buildResponse updatedRqst
    -- (verbose) putStrLn $ "*** RESPONSE ***\n" ++ (show response)
    reply handle response 

-- gets the size of a request body 
getContLen :: Request -> Int
getContLen request@(Request rqst hdr bdy) =
    let len = lookup "Content-Length" hdr
    in case len of 
        (Just n) -> read n :: Int
        (Nothing) -> 0

-- gets the string that correspond to the body of a request
getBody :: Handle -> Int -> IO String -> IO String
getBody handle 0 bdy = bdy
getBody handle len bdy = do 
    c <- hGetChar handle
    cs <- getBody handle (len-1) bdy
    return (c:cs)

takeLines :: [IO String] -> IO [String]
takeLines getLines = do
    x <- head getLines
    case show x of
        "\"\\r\"" -> return []
        "\"\"" -> return []
        _ -> (takeLines (tail getLines)) >>= \xs -> return (x:xs)

parseRequest :: [String] -> Request
parseRequest lns = 
    let rqst = words (head lns)
        uri =  rqst !! 1
        version = rqst !! 2
        hdrs = parseRqstHdrs (tail lns) []
        request :: RequestMethod -> Request
        request m = Request 
                    Rqst { rURI = uri, rMethod = m, rVersion = version } 
                    hdrs
                    []
    in case (head rqst) of
        "GET" -> request GET
        "POST" -> request POST
        "PUT" -> request PUT
        "DELETE" -> request DELETE
        _ -> request UNDEFINED

parseRqstHdrs :: [String] -> RequestHeaders -> RequestHeaders -- [(String,String)]
parseRqstHdrs [] parsedHrds = parsedHrds
parseRqstHdrs (l:lns) parsedHrds = parseRqstHdrs lns $ parsedHrds++[(k,v)]
    where k = takeWhile (/=':') l
          v = tail $ dropWhile (/=' ') l

-- Generate response
buildResponse :: Request -> Response
buildResponse request@(Request rqst hdr bdy) = 
    let contype = "application/json"
        getDate = unsafePerformIO $ fmap show getCurrentTime
    in case (rMethod(rqst)) of 
        GET -> Response
               ResponseHeaders { statusCode = "200", resDate = getDate, resConType = contype, resServer = version}
               ( "{ request }\n" ++ show request )
        POST -> Response
               ResponseHeaders { statusCode = "201", resDate = getDate, resConType = contype, resServer = version}
               ( "{ request }\n" ++ show request )
        PUT -> Response
               ResponseHeaders { statusCode = "200", resDate = getDate, resConType = contype, resServer = version}
               ( "{ request }\n" ++ show request )
        DELETE -> Response
               ResponseHeaders { statusCode = "200", resDate = getDate, resConType = contype, resServer = version}
               ( "{ request }\n" ++ show request )
        UNDEFINED -> Response
               ResponseHeaders { statusCode = "405", resDate = getDate, resConType = contype, resServer = version}
               ( "{ request }\n" ++ show request )

-- Send Response back to client
reply :: Handle -> Response -> IO ()
reply handle rsp@(Response hdr bdy) = do
    hPutStr handle $ show rsp 
    hPutStr handle $ "\n{ response }\n" ++ show hdr
    return ()
