module Main where

import System.Environment
import Control.Monad
import Data.Maybe

import FileManager
import NcxParser
import Unzip
import XhtmlParser

convertDirectory:: FilePath -> IO ()
convertDirectory dir = do
  let outputDir = getOutputDir dir
  files <- findXhtmlFiles dir
  mapM_ (convertFile xhtmlToMarkdown dir outputDir) files

convertTableOfContents:: FilePath -> IO()
convertTableOfContents dir = do
  let outputDir = getOutputDir dir
  maybeTOC <- findTableOfContents dir
  mapM_ (convertFile tableOfContentsToMarkdown dir outputDir) maybeTOC

getEpubFilesDirectory :: FilePath -> IO (Bool, Either Int FilePath)
getEpubFilesDirectory path = do
  fileShouldBeUnzipped <- shouldBeUnzipped path
  if fileShouldBeUnzipped  then do
    unzipResult <- unzipEpub path
    return (True, unzipResult)
  else return (False, Right path)

handleEpubDirectory :: Bool -> Either Int FilePath -> IO ()
handleEpubDirectory _ (Left errorCode) = error $ "Unzip command returned error code " ++ show errorCode
handleEpubDirectory shouldCleanUp (Right dir) = do
  convertDirectory dir
  convertTableOfContents dir
  when shouldCleanUp (cleanup dir)

main :: IO ()
main = do
  [ file ] <- getArgs
  (shouldCleanUp, errorOrDir) <- getEpubFilesDirectory file
  handleEpubDirectory shouldCleanUp errorOrDir
