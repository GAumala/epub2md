import Test.HUnit

import DataTests
import FileManagerTests
import NcxParserTests
import XhtmlParserTests

main :: IO Counts
main = runTestTT $ TestList [
    TestLabel "parseBookHtmlContent" parseBookHtmlContentTest,
    TestLabel "parseTOCHtmlContent" parseTOCHtmlContentTest,
    TestLabel "tagsToMarkdown" tagsToMarkdownTest,
    TestLabel "getInnerXhtml" getInnerXhtmlTest,
    TestLabel "xhtmlToMarkdown" xhtmlToMarkdownTest,
    TestLabel "ncxToMarkdownTest" ncxToMarkdownTest,
    TestLabel "getOutputDir" getOutputDirTest,
    TestLabel "getOutputMarkdownFilePath" getOutputMarkdownFilePathTest,
    TestLabel "getRelativePath" getRelativePathTest
  ]
