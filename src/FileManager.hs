{-# LANGUAGE OverloadedStrings #-}

module FileManager
    ( cleanup,
      convertFile,
      findXhtmlFiles,
      findTableOfContents,
      getRelativePath,
      getOutputDir,
      getOutputMarkdownFilePath,
      readEpubFile,
      saveMarkdownFile,
      setMDExtension
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as IOText
import qualified Data.List as L
import System.FilePath
import System.Directory
import Data.Maybe
import System.FilePath.Find
import Control.Monad

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

isTableOfContents :: FindClause Bool
isTableOfContents = fmap hasTOCName fileName
  where hasTOCName name = name == "toc.ncx"

findXhtmlFiles :: FilePath -> IO [FilePath]
findXhtmlFiles = find honorMaxRecursionLimit isXhtml

findTableOfContents :: FilePath -> IO (Maybe FilePath)
findTableOfContents rootDir = do
    searchResults <- find honorMaxRecursionLimit isTableOfContents rootDir
    if null searchResults then return Nothing
    else return $ Just (head searchResults)

convertFile:: (T.Text -> T.Text) -> FilePath ->  FilePath -> FilePath -> IO()
convertFile converterFunc rootDir outputDir filePath = do
  let relativePath = getRelativePath rootDir filePath
  rawXhtml <- readEpubFile filePath
  let markdownFilePath = getOutputMarkdownFilePath outputDir relativePath
  let markdownOutput = converterFunc rawXhtml
  saveMarkdownFile markdownFilePath markdownOutput

cleanup :: FilePath ->  IO()
cleanup = removeDirectoryRecursive
