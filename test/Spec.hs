import Test.HUnit

import LibTests
import DataTests
import FileManagerTests

main :: IO Counts
main = runTestTT $ TestList [
    TestLabel "parseBookHtmlContent" parseBookHtmlContentTest,
    TestLabel "tagsToMarkdown" tagsToMarkdownTest,
    TestLabel "xhtmlToMarkdown" xhtmlToMarkdownTest,
    TestLabel "getOutputDir" getOutputDirTest,
    TestLabel "getOutputMarkdownFilePath" getOutputMarkdownFilePathTest,
    TestLabel "getRelativePath" getRelativePathTest
  ]
