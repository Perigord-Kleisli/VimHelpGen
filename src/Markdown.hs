{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Markdown
  ( converter
  , MDConvertInfo(..)
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

import           Data.Maybe
import           Numeric.Natural
import           VimHelpSyntax

data MDConvertInfo = MDConvertInfo
  { _lineBreakLength :: Natural
  , _indentLevel     :: Natural
  , _tags            :: [T.Text]
  , _fileName        :: FilePath
  , _moduleName      :: T.Text
  }
  deriving Show
makeLenses ''MDConvertInfo

nodesToVimHelp :: [Node] -> ST.State MDConvertInfo [T.Text]
nodesToVimHelp ((Node _ THEMATIC_BREAK _) : xs) = do
  x <-
    ST.get <&> (`T.replicate` "=") . fromInteger . toInteger . _lineBreakLength
  (x :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (HEADING level) conts) : xs) = do
  (headerText, tagText) <- case conts of
    [] -> return ("", Nothing)
    [Node _ (TEXT text) conts, Node _ (HTML_INLINE tag) _] -> return
      (text, T.strip <$> (T.stripSuffix "-->" =<< T.stripPrefix "<!--" tag))
    x -> (,Nothing) . T.unwords <$> nodesToVimHelp x


  fileName <- T.pack . _fileName <$> ST.get

  heading <- heading (fromIntegral level) . _lineBreakLength <$> ST.get
  let tag = makeTag (fromMaybe headerText tagText) fileName

  case fromIntegral level of
    1 -> do
      indentLevel .= 1
      tags %= (++ [tag])
      (heading headerText tagText :) <$> nodesToVimHelp xs

    2 -> do
      indentLevel .= 2
      tags %= (++ [tag])
      (heading headerText tagText :) <$> nodesToVimHelp xs

    x -> do
      indentLevel .= x
      (heading headerText Nothing :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (TEXT text) conts) : xs) = do
  text' <- T.unwords . ([text] ++) <$> nodesToVimHelp conts
  (text' :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ PARAGRAPH conts): xs) = do
  text <- T.unlines <$> nodesToVimHelp conts
  (text:) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ BLOCK_QUOTE conts) : xs) = do
  text <- T.unlines . map (("▌ " <>) . T.strip) <$> nodesToVimHelp conts
  (text:) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (CODE_BLOCK info conts) _): xs) = do
  (("`" <> T.strip conts <> "`"):) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (CODE text) conts) : xs) = do
  text' <-
    ("`" <>)
    .   (<> "`")
    .   T.unwords
    .   ([text] ++)
    <$> nodesToVimHelp conts
  (text' :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ t conts) : xs) =
  ((T.pack $ show $ Node Nothing t conts) :) <$> nodesToVimHelp xs
nodesToVimHelp [] = return []

-- Generating Text from the file heading
nodeToVimHelp :: MDConvertInfo -> Node -> (T.Text, T.Text)
nodeToVimHelp convertInfo (Node _ DOCUMENT ((Node _ (HEADING _) [Node _ (TEXT title) _]) : (Node _ PARAGRAPH [Node _ (TEXT tagline) _]) : xs))
  = T.unlines . (header :) *** T.unlines . _tags $ ST.runState
    (nodesToVimHelp xs)
    convertInfo
  where header = docHeader (tag $ T.pack $ convertInfo ^. fileName) tagline (convertInfo ^. lineBreakLength)

nodeToVimHelp convertInfo (Node _ DOCUMENT nodes) =
  T.unlines *** T.unlines . _tags $ ST.runState (nodesToVimHelp nodes)
                                                convertInfo


converter :: MDConvertInfo -> T.Text -> (T.Text, T.Text)
converter convertInfo = nodeToVimHelp convertInfo
  . commonmarkToNode [optSmart] [extStrikethrough, extTable, extTaskList]
