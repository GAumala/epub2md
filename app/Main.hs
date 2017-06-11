module Main where

import Lib
import FileManager
import Unzip
import System.Environment
import Control.Monad

convertFile:: FilePath ->  FilePath -> FilePath -> IO()
convertFile rootDir outputDir filePath = do
  let relativePath = getRelativePath rootDir filePath
  rawXhtml <- readEpubFile filePath
  let markdownFilePath = getOutputMarkdownFilePath outputDir relativePath
  let markdownOutput = xhtmlToMarkdown rawXhtml
  saveMarkdownFile markdownFilePath markdownOutput

convertDirectory:: FilePath -> IO ()
convertDirectory dir = do
  let outputDir = getOutputDir dir
  files <- findXhtmlFiles dir
  mapM_ (convertFile dir outputDir) files

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
  when shouldCleanUp (cleanup dir)

main :: IO ()
main = do
  [ file ] <- getArgs
  (shouldCleanUp, errorOrDir) <- getEpubFilesDirectory file
  handleEpubDirectory shouldCleanUp errorOrDir
