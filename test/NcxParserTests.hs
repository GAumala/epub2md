{-# LANGUAGE OverloadedStrings #-}

module NcxParserTests (
    parseTOCHtmlContentTest
) where

import Test.HUnit
import qualified Data.Text as T
import Text.HTML.TagSoup

import NcxParser

parseTOCHtmlContentTest = TestCase (
  assertEqual "Returns an array with the tags contained in the navMap"
    [
      TagOpen "navPoint" [("id", "navPoint-1"), ("playOrder", "1")],
      TagOpen "navLabel" [],
      TagOpen "text" [],
      TagText "Cubierta",
      TagClose "text",
      TagClose "navLabel",
      TagOpen "content" [("src", "Text/cubierta.xhtml")],
      TagClose "content",
      TagClose "navPoint"
    ]

    (parseTOCHtmlContent testXhtml)
  )
  where testXhtml = "<?xml version=\"1.0\" encoding=\"utf-8\"?><!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\"><ncx xmlns=\"http://www.daisy.org/xgrr5/2009/ncx/\" version=\"2006-9\"> <head><meta /> </head> <docTitle> <text>Lorem Ipsum</text></docTitle> <navMap><navPoint id=\"navPoint-1\" playOrder=\"1\"><navLabel><text>Cubierta</text></navLabel><content src=\"Text/cubierta.xhtml\" /></navPoint></navMap></ncx>"
