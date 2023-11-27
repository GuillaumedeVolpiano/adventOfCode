module InputRetrieval () where
    import Data.List.Split ( splitOn )
    import Network.Curl ( CurlOption (CurlProxy, CurlCookie), curlGetString )
    import System.Directory ( getHomeDirectory )

    cookiePath = "/adventOfCode/cookies.json"
    -- cookie = "session=53616c7465645f5f7b0f001bb47c137315f6f3aa5a9df0c8d2bf589168ff4ab3ac934f290e6e11b9a3a1bd1ca9c2a87ec1133b7211f0ee896d0a5c985d7bee27"
    adventURL = "https://adventofcode.com/"
    proxyPath = "/adventOfCode/proxy"

    parseCookie :: String -> IO String
    parseCookie path = do
            rawCookie <- readFile path
            let cookieLength = length rawCookie
            --    ([[_:session:_],[_:value:_]:_]) = filter (\(x:_) -> x == "name" || x == "value") . map (splitOn ":") . splitOn "\n" . filter (\x -> x /= ' ' && x /= '\"') . drop 4 . take (cookieLength - 2) $ rawCookie
            -- return $ session ++ ":" ++ value
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
