module Unzip
    ( unzipEpub,
      shouldBeUnzipped
    ) where

import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Data.Char

getUnzipProcessResult :: ExitCode -> FilePath -> Either Int String
getUnzipProcessResult ExitSuccess path = Right path
getUnzipProcessResult (ExitFailure code) _ = Left code


unzipEpub :: FilePath -> IO (Either Int String)
unzipEpub zipPath = do
  let rootEpubDir = dropExtension zipPath
  (code, stdout, stderr) <- readProcessWithExitCode "unzip"
    ["-d", rootEpubDir, zipPath] ""
  return $ getUnzipProcessResult code rootEpubDir

shouldBeUnzipped :: FilePath -> IO Bool
shouldBeUnzipped filePath = do
  fileExists <- doesFileExist filePath
  let isEpubFile = map toLower (takeExtension filePath) == ".epub"
  return $ fileExists && isEpubFile
