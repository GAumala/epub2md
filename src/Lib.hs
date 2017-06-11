{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseBookHtmlContent,
      tagsToMarkdown,
      xhtmlToMarkdown
    ) where

import qualified Data.Text as T
import Text.HTML.TagSoup

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

processBulletList :: [Tag T.Text] -> T.Text
processBulletList [] = T.empty
processBulletList (TagOpen "li" _ : TagText itemString : TagClose "li" : xs) =
  T.concat ["- ", itemString, "\n", processBulletList xs]
processBulletList (x:xs) = processBulletList xs

processTag :: T.Text -> [Attribute T.Text] -> [Tag T.Text] -> T.Text
processTag tagName _ innerTags
  | isEmpty markdownContent = T.empty
  | tagName == "p" = T.concat [markdownContent, "\n\n"]
  | tagName == "strong" = T.concat ["**", markdownContent, "**"]
  | tagName == "em" = T.concat ["*", markdownContent, "*"]
  | tagName == "i" = T.concat ["*", markdownContent, "*"]
  | tagName == "h1" = T.concat ["# ", markdownContent, "\n\n"]
  | tagName == "h2" = T.concat ["## ", markdownContent, "\n\n"]
  | tagName == "h3" = T.concat ["### ", markdownContent, "\n\n"]
  | tagName == "h4" = T.concat ["#### ", markdownContent, "\n\n"]
  | tagName == "h5" = T.concat ["##### ", markdownContent, "\n\n"]
  | tagName == "h6" = T.concat ["###### ", markdownContent, "\n\n"]
  | tagName == "ul" = T.concat [processBulletList innerTags, "\n"]
  | tagName == "img" = T.empty --ignore images
  | otherwise = markdownContent
  where
    markdownContent = tagsToMarkdown innerTags
    isEmpty = T.null . T.strip

cleanText :: T.Text -> T.Text
cleanText text =  if T.null trimmed then T.empty else text
  where trimmed = T.strip text

tagsToMarkdown :: [Tag T.Text] -> T.Text
tagsToMarkdown [] = T.empty
tagsToMarkdown (TagText text:xs) = T.concat [cleanText text, tagsToMarkdown xs]
tagsToMarkdown (TagOpen name attributes:xs) = T.concat [processTag name attributes tagInnerContent,
    tagsToMarkdown (safeTail remainingTags)]
  where (tagInnerContent, remainingTags) = break (== TagClose name) xs
tagsToMarkdown (x:xs) = tagsToMarkdown xs

xhtmlToMarkdown :: T.Text -> T.Text
xhtmlToMarkdown =  tagsToMarkdown . parseBookHtmlContent
