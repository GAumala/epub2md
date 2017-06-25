{-# LANGUAGE OverloadedStrings #-}

module XhtmlParserTests (
    parseBookHtmlContentTest,
    tagsToMarkdownTest
) where

import Test.HUnit
import qualified Data.Text as T
import Text.HTML.TagSoup

import XhtmlParser

parseBookHtmlContentTest = TestCase (
  assertEqual "Returns an array with the tags contained in the body"
    [TagText "some content"]
    (parseBookHtmlContent testXhtml)
  )
  where testXhtml = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\"><head><title>Title of document</title></head><body>some content</body></html>"

tagsToMarkdownTest = TestCase (
  assertEqual "Converts 2 paragraph tags to markdown"
    "This is first paragraph.\n\nThis is second paragraph.\n\n"
    (tagsToMarkdown [TagOpen "p" [], TagText "This is first paragraph.", TagClose "p", TagOpen "p" [], TagText "This is second paragraph.", TagClose "p"])
    >>
  assertEqual "Converts a paragraph with italics to markdown"
    "This paragraph contains *italics*.\n\n"
    (tagsToMarkdown [TagOpen "p" [], TagText "This paragraph contains ", TagOpen "em" [], TagText "italics", TagClose "em", TagText ".", TagClose "p"])
    >>
  assertEqual "Converts a paragraph with bold to markdown"
    "This paragraph contains **bold**.\n\n"
    (tagsToMarkdown [TagOpen "p" [], TagText "This paragraph contains ", TagOpen "strong" [], TagText "bold", TagClose "strong", TagText ".", TagClose "p"])
    >>
  assertEqual "Converts a header and a list"
    "# Title\n\n- item 1\n- item 2\n\n"
    (tagsToMarkdown [TagOpen "h1" [], TagText "Title", TagClose "h1", TagOpen "ul" [], TagOpen "li" [], TagText "item 1", TagClose "li", TagOpen "li" [], TagText "item 2", TagClose "li", TagClose "ul"])
  )
