module DataDef where
-- Defines the structures of Requests and Responses

data RequestMethod = GET 
                   | POST 
                   | PUT
                   | DELETE
                   | UNDEFINED
                   deriving (Show, Eq)

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

data Response = Response ResponseHeaders ResponseBody

instance Show ResponseHeaders where
    show resp = "HTTP/1.1 " ++ statusCode(resp) ++ reasonPhrase(statusCode(resp)) ++ "\nDate: " ++ resDate(resp) ++ "\nContent-Type: " ++ resConType(resp)

instance Show Response where
    show (Response headers body) = show(headers) ++ "\n\n" ++ body

instance Show Request where
    show (Request headers body) = show(headers) ++ "\n" ++ show(body)

instance Show RequestHeaders where
    show rHeaders = "Content-type: " ++ reqConType(rHeaders) ++ "\nAccept: " ++ rAccept(rHeaders)

instance Show RequestBody where
    show rBody = "URI: " ++ rURI(rBody) ++ "\nRequest Method: " ++ show(rMethod(rBody)) ++ "\nParams: " ++ show(rParams(rBody)) 

stringToMethod :: String -> RequestMethod
stringToMethod s = case s of
    "GET" -> GET
    "POST" -> POST
    "PUT" -> PUT
    "DELETE" -> DELETE
    _ -> UNDEFINED

reasonPhrase :: String -> String
reasonPhrase statusCode = case statusCode of
    "100" -> " Continue"
    "101" -> " Switching Protocols"
    "200" -> " OK"
    "201" -> " Created"
    "202" -> " Accepted"
    "203" -> " Non-Authoritative Information"
    "204" -> " No Content"
    "205" -> " Reset Content"
    "206" -> " Partial Content"
    "300" -> " Multiple Choices"
    "301" -> " Moved Permanently"
    "302" -> " Found"
    "303" -> " See Other"
    "304" -> " Not Modified"
    "305" -> " Use Proxy"
    "307" -> " Temporary Redirect"
    "400" -> " Bad Request"
    "401" -> " Unauthorized"
    "402" -> " Payment Required"
    "403" -> " Forbidden"
    "404" -> " Not Found"
    "405" -> " Method Not Allowed"
    "406" -> " Not Acceptable"
    "407" -> " Proxy Authentication Required"
    "408" -> " Request Time-out"
    "409" -> " Conflict"
    "410" -> " Gone"
    "411" -> " Length Required"
    "412" -> " Precondition Failed"
    "413" -> " Request Entity Too Large"
    "414" -> " Request-URI Too Large"
    "415" -> " Unsupported Media Type"
    "416" -> " Requested range not satisfiable"
    "417" -> " Expectation Failed"
    "500" -> " Internal Server Error"
    "501" -> " Not Implemented"
    "502" -> " Bad Gateway"
    "503" -> " Service Unavailable"
    "504" -> " Gateway Time-out"
    "505" -> " HTTP Version not supported"

