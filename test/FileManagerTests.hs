{-# LANGUAGE OverloadedStrings #-}

module FileManagerTests (
  getOutputDirTest,
  getOutputMarkdownFilePathTest,
  getRelativePathTest
) where

import Test.HUnit
import qualified Data.Text as T

import FileManager

getOutputDirTest = TestCase (
  assertEqual "should return the path to the output dir, next to root dir"
    [
      "epub-files-md",
      "../../someDir/epub-files-md",
      "dir/epub-files-md",
      "./epub-files-md"
    ]
    [
      getOutputDir "epub-files",
      getOutputDir "../../someDir/epub-files",
      getOutputDir "dir/epub-files",
      getOutputDir "./epub-files/"
    ]
  )

getRelativePathTest = TestCase (
  assertEqual "should return the path relative to the root, without preceding dot or slashes"
    [
      "file.md",
      "text/file.md",
      "text/file.md"
    ]
    [
      getRelativePath "epub-files" "epub-files/file.md",
      getRelativePath "epub-files" "epub-files/text/file.md",
      getRelativePath "../some/dir/epub-files" "../some/dir/epub-files/text/file.md"
    ]
  )
getOutputMarkdownFilePathTest = TestCase (
  assertEqual "should join the output dir path and the relative filepath"
    [
      "epub-files-md/file.md",
      "epub-files-md/file.md",
      "./epub-files-md/file.md",
      "./epub-files-md/text/file.md",
      "../some/dir/epub-files-md/text/file.md"
    ]
    [
      getOutputMarkdownFilePath "epub-files-md" "file.xhtml",
      getOutputMarkdownFilePath "epub-files-md/" "file.xhtml",
      getOutputMarkdownFilePath "./epub-files-md" "file.xhtml",
      getOutputMarkdownFilePath "./epub-files-md" "text/file.xhtml",
      getOutputMarkdownFilePath "../some/dir/epub-files-md/" "text/file.xhtml"
    ]
  )
