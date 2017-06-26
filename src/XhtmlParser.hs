{-# LANGUAGE OverloadedStrings #-}

module XhtmlParser
    ( getInnerXhtml,
      parseBookHtmlContent,
      tagsToMarkdown,
      xhtmlToMarkdown
    ) where

import qualified Data.Text as T
import Text.HTML.TagSoup

import ParserCommons

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

isNotBodyOrHtmlTag :: Tag T.Text -> Bool
isNotBodyOrHtmlTag (TagOpen "body" _) = False
isNotBodyOrHtmlTag (TagClose "body") = False
isNotBodyOrHtmlTag (TagClose "html") = False
isNotBodyOrHtmlTag x = True

parseBookHtmlContent :: T.Text -> [Tag T.Text]
parseBookHtmlContent xhtmlString = filter isNotBodyOrHtmlTag tagList
  where
    bodyTag = "<body>" :: String
    bodyHtmlContent = partitions (~== bodyTag) (parseTags xhtmlString)
    tagList = head bodyHtmlContent

processBulletList :: [Tag T.Text] -> Int -> T.Text
processBulletList [] _ = T.empty
processBulletList (TagOpen "li" _ : TagOpen "ul" _: xs) level =
  T.concat [processBulletList nestedListInnerXhtml (level + 1), processBulletList otherTags level ]
  where (nestedListInnerXhtml, otherTags) = getInnerXhtml "ul" xs
processBulletList (TagOpen "li" _ : xs) level =
  T.concat [spacesForLevel level, "- ", tagsToMarkdown listItemInnerXhtml, "\n", processBulletList xs level]
  where (listItemInnerXhtml, otherTags) = getInnerXhtml "li" xs
processBulletList (x:xs) level = processBulletList xs level

processTag :: T.Text -> [Attribute T.Text] -> [Tag T.Text] -> T.Text
processTag tagName _ innerTags
  | isEmpty markdownContent = T.empty
  | tagName == "p" = T.concat [markdownContent, "\n\n"]
  | tagName == "strong" = T.concat ["**", markdownContent, "**"]
  | tagName == "em" = T.concat ["*", markdownContent, "*"]
  | tagName == "i" = T.concat ["*", markdownContent, "*"]
  | tagName == "code" = T.concat ["`", markdownContent, "`"]
  | tagName == "h1" = T.concat ["# ", markdownContent, "\n\n"]
  | tagName == "h2" = T.concat ["## ", markdownContent, "\n\n"]
  | tagName == "h3" = T.concat ["### ", markdownContent, "\n\n"]
  | tagName == "h4" = T.concat ["#### ", markdownContent, "\n\n"]
  | tagName == "h5" = T.concat ["##### ", markdownContent, "\n\n"]
  | tagName == "h6" = T.concat ["###### ", markdownContent, "\n\n"]
  | tagName == "ul" = T.concat [processBulletList innerTags 0, "\n"]
  | tagName == "img" = T.empty --ignore images
  | otherwise = markdownContent
  where
    markdownContent = tagsToMarkdown innerTags
    isEmpty = T.null . T.strip

cleanText :: T.Text -> T.Text
cleanText " " = " "
cleanText text =  if T.null trimmed then T.empty else text
  where trimmed = T.strip text

getInnerXhtml' :: T.Text -> [Tag T.Text] -> [Tag T.Text] -> Int -> ([Tag T.Text], [Tag T.Text])
getInnerXhtml' tagName innerTags (TagClose tagCloseName:xs) level
  | tagCloseName == tagName && level == 0 = (innerTags, xs)
  | tagCloseName == tagName && level > 0 = getInnerXhtml' tagName (innerTags ++ [TagClose tagCloseName]) xs (level - 1)
  | otherwise = getInnerXhtml' tagName (innerTags ++ [TagClose tagCloseName]) xs level
getInnerXhtml' tagName innerTags (TagOpen tagOpenName attrs:xs) level
  | tagOpenName == tagName = getInnerXhtml' tagName (innerTags ++ [TagOpen tagOpenName attrs]) xs (level + 1)
  | otherwise = getInnerXhtml' tagName (innerTags ++ [TagOpen tagOpenName attrs]) xs level
getInnerXhtml' tagName innerTags (x:xs) level = getInnerXhtml' tagName (innerTags ++ [x]) xs level

getInnerXhtml :: T.Text -> [Tag T.Text] -> ([Tag T.Text], [Tag T.Text])
getInnerXhtml tagName tags = getInnerXhtml' tagName [] tags 0

tagsToMarkdown :: [Tag T.Text] -> T.Text
tagsToMarkdown [] = T.empty
tagsToMarkdown (TagText text:xs) = T.concat [cleanText text, tagsToMarkdown xs]
tagsToMarkdown (TagOpen name attributes:xs) = T.concat [processTag name attributes tagInnerContent,
    tagsToMarkdown remainingTags]
  where (tagInnerContent, remainingTags) = getInnerXhtml name xs
tagsToMarkdown (x:xs) = tagsToMarkdown xs

xhtmlToMarkdown :: T.Text -> T.Text
xhtmlToMarkdown =  tagsToMarkdown . parseBookHtmlContent
