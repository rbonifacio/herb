module DataDef where
-- Defines the structures of Requests and Responses

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

data Response = Response ResponseHeaders ResponseBody

instance Show ResponseHeaders where
    show resp = "Status Code: " ++ statusCode(resp) ++ "\nDate: " ++ resDate(resp) ++ "\nContent-Type:" ++ resConType(resp) ++ "\n"

instance Show Response where
    show (Response headers body) = show(headers) ++ body ++ "\n"

