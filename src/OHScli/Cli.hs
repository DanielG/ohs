module Main where

import Control.Monad.IO.Class
import System.Environment
import System.Console.GetOpt (getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..))
import Network.URI
import OHS.Client

main = do
  argv <- getArgs

  let (Options host port, rest) =
          case getOpt Permute globalArgSpec argv of
            (o,r,[]  ) -> (foldr id (Options "localhost" "1234") o, r)
            (_,_,errs) -> error $
              "Parsing command line options failed: " ++ concat errs

  case (rest) of
    ["login", uri, uid, ua] -> do
        let Just uri' = parseURI uri
        print =<< runOHST host port (login uri' (UId uid))
    ["register", _url, _locator, _cookies] -> print =<< runOHST host port register -- mock
    "login":_ -> putStrLn $ usage "login"
    "register":_ -> putStrLn $ usage "register"
    _ -> putStrLn "Usage: login | register ..."

usage "login" = "Usage: login URL USER_ID USER_AGENT"
usage "register" = "Usage: register URL LOCATOR COOKIES"

option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
option s l udsc dsc = Option s l dsc udsc

reqArg :: String -> (String -> a) -> ArgDescr a
reqArg udsc dsc = ReqArg dsc udsc

data Options = Options {
      optServer :: String
    , optPort   :: String
    }

globalArgSpec :: [OptDescr (Options -> Options)]
globalArgSpec =
      [ option "" ["server"] "Server to connect to" $
               reqArg "SERVER" $ \s o -> o { optServer = s }
      , option "" ["port"] "TCP port to connect to" $
               reqArg "PORT" $ \s o -> o { optPort = s }

  ]
