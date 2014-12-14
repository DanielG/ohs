{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad.List
import Data.List
import Data.List.Split
import System.FilePath
import System.Directory
import System.Environment
import System.Console.GetOpt
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Data.Tree.NavigatableTree.Class
import Data.Tree.NTree.Zipper.TypeDefs

import OHS.Types
import OHS.Locator

data Options = Options {
      oDir :: FilePath
    }

defaultOptions = Options ""

option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
option s l udsc dsc = Option s l dsc udsc

reqArg :: String -> (String -> a) -> ArgDescr a
reqArg udsc dsc = ReqArg dsc udsc

globalArgSpec :: [OptDescr (Options -> Options)]
globalArgSpec =
      [ option "d" ["dir"] "data dirctory"$
               reqArg "DIR" $ \d o -> o { oDir = d }
  ]

main = do
  argv <- getArgs
  let (opts, args) = case getOpt RequireOrder globalArgSpec argv of
                       (o,r,[]  ) -> (foldr id defaultOptions o, r)
                       (_,_,errs) -> error $
                         "Parsing command line options failed: " ++ concat errs

  let command:[] = args

  runListT $ do
    Site {..} <- getSites (oDir opts)

    case command of
      "form-locators" -> do
        let loc = updateFormLocator sHtml sLoginFormXPath
        liftIO $ writeFile (sFile <.> "login-form-locator") (show loc)

--  print =<< map fst <$> getSites dir

updateFormLocator :: String -> String -> [Locator]
updateFormLocator html xpath = runLA (hread >>> locatorFromXpath xpath) html

locatorFromXpath :: forall a. (ArrowXml a) => String -> a XmlTree Locator
locatorFromXpath xpath = arr fromTree >>>
    (getFromNodeSet $< (arr toTree >>> getXPathNodeSet xpath)) >>> locator'

data Site = Site { sFile :: FilePath
                 , sUrl :: String
                 , sHtml :: String
                 , sLoginFormXPath :: String }
          deriving (Read, Show)

getSites :: FilePath -> ListT IO Site
getSites d = do
  let filt = filter (not . ((flip elem [".","..","Update.hs"]) &&& pred >>> uncurry (||)))
  fn <- (d </>) <$> ListT (filt <$> getDirectoryContents d)
  f <- liftIO $ readFile $  fn
  x <- liftIO $ readFile $ fn <.> "login-form-xpath"
  -- TODO: get site url directly from git annex this is broken
  return $ Site fn (replace "_" "/" fn) f x

 where replace n h = (intercalate h . splitOn n)
       pred f = "login-form-xpath" `isSuffixOf` f
                || "login-form-locator" `isSuffixOf` f
