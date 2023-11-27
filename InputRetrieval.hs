module InputRetrieval ( retrieveInput ) where
    import Data.List.Split ( splitOn )
    import Network.Curl ( CurlOption (CurlProxy, CurlCookie), curlGetString )
    import System.Directory ( getHomeDirectory )

    cookiePath = "/adventOfCode/cookies.json"
    adventURL = "https://adventofcode.com/"
    proxyPath = "/adventOfCode/proxy"

    parseCookie :: String -> IO String
    parseCookie path = do
            rawCookie <- readFile path
            let cookieLength = length rawCookie
            let (name:value:_) = map (\(_:y:_) -> init y) . filter (\(x:_) -> x == "name" || x == "value") . map (splitOn ":") . splitOn "\n" . filter (\x -> x /= ' ' && x /= '\"') . drop 4 . take (cookieLength - 2) $ rawCookie 
            return $ name ++ "=" ++ value

    retrieveInput :: Int -> Int -> IO String
    retrieveInput year day = do
            let url = adventURL ++ show year ++ "/day/" ++ show day ++ "/input"
            home <- getHomeDirectory
            cookie <- parseCookie $  home ++ cookiePath
            proxy <- readFile (home ++ proxyPath)
            (code, rsp) <- curlGetString url [CurlProxy (init proxy), CurlCookie cookie] 
            print code
            return rsp
