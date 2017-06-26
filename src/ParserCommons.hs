{-# LANGUAGE OverloadedStrings #-}

module ParserCommons
    ( spacesForLevel
    ) where

import qualified Data.Text as T

spacesForLevel:: Int -> T.Text
spacesForLevel 0 = T.empty
spacesForLevel level = T.concat $ replicate (level * 2) " "
