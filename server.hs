{-# LANGUAGE DoAndIfThenElse #-}

module Server where
-- HERB server's functions 

import System.IO (Handle(..), hPutStr, hGetLine, hGetChar)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time (getCurrentTime)
import System.Posix.Files.ByteString (getFileStatus, isDirectory, fileExist)
import System.Directory (getDirectoryContents)
import Data.Aeson (toJSON, ToJSON(..), (.=), object, encode)
import qualified Data.Text as T (Text(..), pack)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)

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
    processedRqst@(code,body) <- proccessRequest updatedRqst
    let response = buildResponse processedRqst
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

proccessRequest :: Request -> IO (String, ResponseBody)
proccessRequest (Request rqst hdr bdy) =
    case rMethod rqst of
        GET -> proccessGET $ tail $ rURI rqst
        POST -> proccessPOST (tail $ rURI(rqst)) hdr bdy
        PUT -> undefined 
        DELETE -> undefined
        UNDEFINED -> return ("405",reasonPhrase "405")

-- proccessGET :: URI -> ( Status Code , ResponseBody )
proccessGET :: String -> IO (String, ResponseBody)
proccessGET resource = do
    let filePath = "./resources/" ++ resource
    let rFilePath = B.pack filePath
    resourceExist <- fileExist rFilePath
    if resourceExist
        then do
            fileStatus <- getFileStatus rFilePath
            if isDirectory fileStatus
                then do
                    dirContents <- getDirectoryContents filePath
                    if (resource == "")
                        then do
                            let path = resource
                            let resources = map (path ++) $ drop 2 dirContents
                            rTyped <- resourcesTyped resources []
                            return ("200", resourcesToJSON rTyped)
                        else do
                            let path = resource ++ "/"
                            let resources = map (path ++) $ drop 2 dirContents
                            rTyped <- resourcesTyped resources []
                            return ("200", resourcesToJSON rTyped)
                else do
                    content <- readFile filePath
                    return ("200", content)
        else return ("404", reasonPhrase "404")

resourcesTyped :: [String] -> [Resource] -> IO [Resource]
resourcesTyped [] accum = return accum
resourcesTyped (r:rs) accum = do
    let filePath = "./resources/" ++ r
    let rFilePath = B.pack filePath
    fileStatus <- getFileStatus rFilePath
    if isDirectory fileStatus
        then do
            resourcesTyped rs ((Set { sName = T.pack r }):accum)
        else do 
            resourcesTyped rs ((Event { eName = T.pack r }):accum)

resourcesToJSON :: [Resource] -> String
resourcesToJSON r = BL.unpack $ encode r

proccessPOST :: String -> RequestHeaders -> RequestBody -> IO (String, ResponseBody)
proccessPOST resource hdrs bdy = do
    writeFile ("./resources/" ++ resource) ((rqstHdrsToString hdrs []) ++ bdy)
    return ("201",reasonPhrase "201")

-- Generate response
buildResponse :: (String, ResponseBody) -> Response
buildResponse (code,bdy) = 
    let contype = "application/json"
        getDate = unsafePerformIO $ fmap show getCurrentTime
    in (Response
        ResponseHeaders { statusCode = code, resDate = getDate, resConType = contype, resServer = version}
        bdy)

-- Send Response back to client
reply :: Handle -> Response -> IO ()
reply handle rsp@(Response hdr bdy) = do
    hPutStr handle $ show rsp 
    -- (debug) hPutStr handle $ "\n{ response }\n" ++ show hdr
