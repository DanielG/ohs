{-# LANGUAGE RecordWildCards, ScopedTypeVariables, BangPatterns #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.List
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.List
import Data.List.Split
import System.IO
import System.FilePath
import System.Directory
import System.Environment
import System.Console.GetOpt
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Data.Tree.NavigatableTree.Class
import Data.Tree.NTree.Zipper.TypeDefs

import Network.URI
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.ByteString as BS
import qualified Data.Traversable as T

import OHS.Types
import OHS.Locator
import OHS.HTTP

data Options = Options {
      oDir :: FilePath
    , oRS  :: String
    , oFS  :: String
    } deriving (Show)

defaultOptions = Options "." "\n" "\t"

option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
option s l udsc dsc = Option s l dsc udsc

reqArg :: String -> (String -> a) -> ArgDescr a
reqArg udsc dsc = ReqArg dsc udsc

globalArgSpec :: [OptDescr (Options -> Options)]
globalArgSpec =
 [ option "d" ["dir"] "data dirctory"$
     reqArg "DIR" $ \d o -> o { oDir = d }

 , option ""  ["rs"] "output record seperator" $
     reqArg "ORS" $ \s o -> o { oRS = read $ "\""++s++"\"" }

 , option ""  ["fs"] "output field seperator" $
     reqArg "OFS" $ \s o -> o { oFS = read $ "\""++s++"\"" }

 , option ""  ["null"] "separate output records using a null character ('\0')" $
     NoArg $ \o -> o { oRS = "\0" }
 ]

main = do
  argv <- getArgs

  let (opts, args) =
          case getOpt Permute globalArgSpec argv of
            (o,r,[]  ) -> (foldr id defaultOptions o, r)
            (_,_,errs) -> error $
                "Parsing command line options failed: " ++ concat errs
  let
      putRecord r = putStr $ r ++ (oRS opts)
      makeRecord fs = intercalate (oFS opts) fs

      output :: Either String String -> IO ()
      output ea =
          case ea of
            Left err -> hPutStrLn stderr $ err
            Right a -> putRecord a


  let command:cmdArgs = args

  case command of
    "locator-from-xpath" -> do
        let !([xpath]) = cmdArgs
        putRecord =<< (show . locatorFromXPath xpath <$> getContents)

    "list-sites" -> mapM_ output =<< (runListT $ runExceptT $ do
        let ![] = cmdArgs
        Site {..} <- getSites (oDir opts)
        return $ makeRecord [sName, sUri])

    "download" -> do
        let ![url] = cmdArgs
        withManager tlsManagerSettings $ \mngr -> do
          req <- parseUrl url
          withBrowser mngr req $ \((req,res):rs) -> BS.putStr $ responseBody res

 where


--  print =<< map fst <$> getSites dir

locatorFromXPath :: String -> String -> [Locator]
locatorFromXPath xpath html = runLA (hread >>> locatorFromXPath' xpath) html

locatorFromXPath' :: forall a. (ArrowXml a) => String -> a XmlTree Locator
locatorFromXPath' xpath = arr fromTree >>>
    (getFromNodeSet $< (arr toTree >>> getXPathNodeSet xpath)) >>> locator'

data Site = Site { sName :: String
                 , sDir :: FilePath
                 , sUri :: String
                 }
          deriving (Read, Show)

-- TODO: allow sub directories maybe?
getSites :: FilePath -> ExceptT String (ListT IO) Site
getSites d = do
  sites <- liftIO $ filterM doesDirectoryExist =<< getDirectoryContents' d

  let siteDirs = (d </>) `map` sites
  (site, dir) <- lift $ ListT $ return (sites `zip` siteDirs)

  let readSite f = liftIO $ readFile $ (dir </>) f

  uri <- handleMaybe (throwE $ "Can't parse login-url for site `"++ site ++"'") $
             parseURI <$> readSite "login-url"

  return $ Site site dir (show uri)

 where
   handleMaybe :: MonadError e m => m a -> m (Maybe a) -> m a
   handleMaybe fail a = do
               ma' <- a
               case ma' of
                 Nothing -> fail
                 Just a' -> return a'

   getDirectoryContents' d =
       filter (not . flip elem [".", ".."]) <$> liftIO (getDirectoryContents d)
