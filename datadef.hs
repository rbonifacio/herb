module DataDef where
-- Defines the structures of Requests and Responses

data RequestMethod = GET 
                   | POST 
                   | PUT
                   | DELETE
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
    show resp = "Status Code: " ++ statusCode(resp) ++ "\nDate: " ++ resDate(resp) ++ "\nContent-Type: " ++ resConType(resp)

instance Show Response where
    show (Response headers body) = show(headers) ++ "\n" ++ body

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

