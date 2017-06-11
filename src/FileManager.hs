{-# LANGUAGE OverloadedStrings #-}

module FileManager
    ( cleanup,
      findXhtmlFiles,
      getRelativePath,
      getOutputDir,
      getOutputMarkdownFilePath,
      readEpubFile,
      saveMarkdownFile,
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as IOText
import qualified Data.List as L
import System.FilePath
import System.Directory
import Data.Maybe
import System.FilePath.Find

maxRecursionLimit = 3

readEpubFile = IOText.readFile

saveMarkdownFile :: FilePath -> T.Text -> IO ()
saveMarkdownFile filepath content = do
  let directory = takeDirectory filepath
  createDirectoryIfMissing True directory
  IOText.writeFile filepath content

honorMaxRecursionLimit :: FindClause Bool
honorMaxRecursionLimit = fmap isBelowLimit depth
  where isBelowLimit level = level <= maxRecursionLimit

getRelativePath :: FilePath -> FilePath -> FilePath
getRelativePath = makeRelative

setMDExtension :: FilePath -> FilePath
setMDExtension path = replaceExtension path ".md"

getOutputDir :: FilePath -> FilePath
getOutputDir rootDir = outputDir
  where
    endsWithSlash = last rootDir == '/'
    rootDirName = if endsWithSlash then takeDirectory rootDir else takeFileName rootDir
    outputDirName = rootDirName ++ "-md"
    replaceRootNameWith = if endsWithSlash then replaceDirectory rootDir else replaceFileName rootDir
    outputDir = replaceRootNameWith outputDirName

getOutputFilePath :: FilePath -> FilePath -> FilePath
getOutputFilePath outputDir relativePath = joinPath [outputDir, relativePath ]

getOutputMarkdownFilePath :: FilePath -> FilePath -> FilePath
getOutputMarkdownFilePath outputDir relativePath = setMDExtension $ getOutputFilePath outputDir relativePath

isXhtml :: FindClause Bool
isXhtml = fmap hasXhtmlExtension extension
  where hasXhtmlExtension ext = ext == ".xhtml"

findXhtmlFiles :: FilePath -> IO [FilePath]
findXhtmlFiles = find honorMaxRecursionLimit isXhtml

cleanup :: FilePath ->  IO()
cleanup = removeDirectoryRecursive
