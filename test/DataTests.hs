{-# LANGUAGE OverloadedStrings #-}

module DataTests (
  ncxToMarkdownTest,
  xhtmlToMarkdownTest
) where

import Test.HUnit
import qualified Data.Text as T

import FileManager
import NcxParser
import XhtmlParser

assertSampleDataEqual:: String -> FilePath -> FilePath -> Assertion
assertSampleDataEqual message sampleMarkdownFilePath sampleXhtmlFilePath = do
  expectedMarkdown <- readEpubFile sampleMarkdownFilePath
  rawXhtml <- readEpubFile sampleXhtmlFilePath
  let actualMarkdown = xhtmlToMarkdown rawXhtml
  assertEqual message expectedMarkdown actualMarkdown

assertSampleTOCEqual:: String -> FilePath -> FilePath -> Assertion
assertSampleTOCEqual message sampleMarkdownFilePath sampleNcxFilePath = do
  expectedMarkdown <- readEpubFile sampleMarkdownFilePath
  rawXhtml <- readEpubFile sampleNcxFilePath
  let actualMarkdown = tableOfContentsToMarkdown rawXhtml
  assertEqual message expectedMarkdown actualMarkdown

xhtmlToMarkdownTest = TestCase (
  assertSampleDataEqual "should convert sample1.xhtml correctly"
    "./test/data/sample1.md"
    "./test/data/sample1.xhtml"
    >>
  assertSampleDataEqual "should convert sample2.xhtml correctly"
    "./test/data/sample2.md"
    "./test/data/sample2.xhtml"
  )

ncxToMarkdownTest = TestCase (
  assertSampleTOCEqual "should convert table of contents correctly"
    "./test/data/tocsample1.md"
    "./test/data/tocsample1.ncx"
  )
