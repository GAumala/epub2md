{-# LANGUAGE OverloadedStrings #-}

module NcxParser
    ( parseTOCHtmlContent,
      tableOfContentsToMarkdown,
    ) where

import qualified Data.Text as T
import Data.List
import Text.HTML.TagSoup

isInterestingTag :: Tag T.Text -> Bool
isInterestingTag (TagOpen "navMap" _) = False
isInterestingTag (TagClose "navMap") = False
isInterestingTag (TagClose "ncx") = False
isInterestingTag (TagText text)
  | T.null trimmed = False
  | otherwise = True
  where trimmed = T.strip text
isInterestingTag x = True

parseTOCHtmlContent :: T.Text -> [Tag T.Text]
parseTOCHtmlContent xhtmlString = filter isInterestingTag tagList
  where
    navMapTag = "<navMap>" :: String
    bodyHtmlContent = partitions (~== navMapTag) (parseTags xhtmlString)
    tagList = head bodyHtmlContent

getPageUrlFromAttrs :: [(T.Text,  T.Text)] -> T.Text
getPageUrlFromAttrs [] = T.empty
getPageUrlFromAttrs(("src", url):xs) = url
getPageUrlFromAttrs (x:xs) = getPageUrlFromAttrs xs

spacesForLevel:: Int -> T.Text
spacesForLevel 0 = T.empty
spacesForLevel level = T.concat $ replicate (level * 2) " "

tagsToMarkdown :: Int -> [Tag T.Text] -> T.Text
tagsToMarkdown _ [] = T.empty
tagsToMarkdown level (TagClose "navPoint":xs) = tagsToMarkdown (level - 1) xs
tagsToMarkdown level (
  TagOpen "navPoint" _:
    TagOpen "navLabel" _:
      TagOpen "text" _:
        TagText chapterTitle:
      TagClose "text":
    TagClose "navLabel":
    TagOpen "content" attrs:
    TagClose "content":
  xs) = T.concat [
    spacesForLevel level,
    "- [", chapterTitle, "](",
    getPageUrlFromAttrs attrs, ")\n",
    tagsToMarkdown (level + 1) xs
  ]
tagsToMarkdown level (x:xs) = tagsToMarkdown level xs

tableOfContentsToMarkdown :: T.Text -> T.Text
tableOfContentsToMarkdown = tagsToMarkdown 0 . parseTOCHtmlContent
