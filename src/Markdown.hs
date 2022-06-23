{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Markdown
  ( converter
  , Indent(..)
  , ConvertInfo(..)
  , fileName
  ) where

import           CMarkGFM
import qualified Control.Monad.State.Lazy      as ST
import qualified Data.Text                     as T
import           Lens.Micro.Platform

import           Control.Arrow
import           Data.Bits                      ( Bits(xor) )
import           Data.Char
import           Data.Function
import           GHC.Read
import           Text.Printf
import           Text.Read

data Indent = Spaces | Tabs
  deriving (Show)

instance Read Indent where
  readPrec = parens $ do
    Ident s <- lexP
    fuzzyMatch s
   where
    fuzzyMatch x | map toLower x == "tabs"   = return Tabs
                 | map toLower x == "spaces" = return Spaces
                 | otherwise                 = pfail


data ConvertInfo = ConvertInfo
  { _lineBreakLength :: Int
  , _indentType      :: Indent
  , _indentLevel     :: Int
  , _tags            :: [T.Text]
  , _fileName        :: FilePath
  , _moduleName      :: T.Text
  }
  deriving Show
makeLenses ''ConvertInfo

makeTag :: T.Text -> T.Text -> T.Text
makeTag tagName fileName =
  T.intercalate "\t" [tagName, fileName, mconcat ["/*", tagName, "*"]]

nodesToVimHelp :: [Node] -> ST.State ConvertInfo [T.Text]
nodesToVimHelp ((Node _ THEMATIC_BREAK _) : xs) = do
  x <- ST.get <&> ((`T.replicate` "=") . _lineBreakLength)
  (x :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (HEADING level) conts) : xs) = do
  headerText <- T.unwords <$> nodesToVimHelp conts
  fileName   <- T.pack . _fileName <$> ST.get
  case level of
    1 -> do
      indentLevel .= 1
      tags %= (++ [makeTag headerText fileName])
      breakWidth <-
        (\x -> T.replicate (x - T.length headerText - 2) " " `mappend` headerText)
        .   _lineBreakLength
        <$> ST.get
      (headerText :) <$> nodesToVimHelp xs

    2 -> do
      indentLevel .= 2 
      tags %= (++ [makeTag headerText fileName])
      (headerText :) <$> nodesToVimHelp xs

    x -> do
      indentLevel .= x
      (headerText :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (TEXT text) conts) : xs) = do
  text' <- T.unwords . ([text] ++) <$> nodesToVimHelp conts
  (text' :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (CODE text) conts) : xs) = do
  text' <-
    ("`" `mappend`) . (`mappend` "`") . T.unwords . ([text] ++) <$> nodesToVimHelp conts
  (text' :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ t conts) : xs) =
  ((T.pack $ printf "%s:    %s" (show t) (show conts)) :) <$> nodesToVimHelp xs
nodesToVimHelp [] = return []



nodeToVimHelp :: ConvertInfo -> Node -> (T.Text, T.Text)
nodeToVimHelp convertInfo (Node _ DOCUMENT nodes) =
  T.unlines *** (T.unlines . _tags) $ ST.runState (nodesToVimHelp nodes) convertInfo


converter :: ConvertInfo -> T.Text -> (T.Text, T.Text)
converter convertInfo = nodeToVimHelp convertInfo
  . commonmarkToNode [optSmart] [extStrikethrough, extTable, extTaskList]
