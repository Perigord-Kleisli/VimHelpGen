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
import           Control.Lens
import           System.FilePath.Posix

import           Control.Arrow
import           Data.Bits                      ( Bits(xor) )
import           Data.Char
import           Data.Function
import           GHC.Read
import           Text.Printf
import           Text.Read

import           Data.Maybe
import           GHC.IO
import           Numeric.Natural
import           VimHelpSyntax
import Data.List

data MDConvertInfo = MDConvertInfo
  { _lineBreakLength :: Natural
  , _indentLevel     :: Natural
  , _tags            :: [T.Text]
  , _tagLine         :: T.Text
  , _unicode         :: Bool
  , _fileName        :: FilePath
  , _breakText       :: Bool
  , _moduleName      :: T.Text
  }
  deriving Show
makeLenses ''MDConvertInfo

log :: Show a => a -> a
log x = unsafePerformIO $ print x >> return x

nodesToVimHelp :: [Node] -> ST.State MDConvertInfo [T.Text]

nodesToVimHelp (x: (Node _ SOFTBREAK _): xs) = 
  (ix 0 %~)
  <$> ((<>) . (<> " ") . head <$> nodesToVimHelp [x]) -- Becomes equal to (<> (x <> " "))
  <*> nodesToVimHelp xs

nodesToVimHelp ((Node _ THEMATIC_BREAK _) : xs) = do
  x <- ST.get <&> (`T.replicate` "=") . fromIntegral . _lineBreakLength
  (x :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (HEADING level) conts) : xs) = do
  (headerText, tagText) <- case conts of
    [] -> return ("", Nothing)
    [Node _ (TEXT text) conts, Node _ (HTML_INLINE tag) _] ->
      return (text, T.strip <$> (T.stripSuffix "-->" =<< T.stripPrefix "<!--" tag))
    x -> (, Nothing) . T.unwords <$> nodesToVimHelp x


  fileName <- T.pack . _fileName <$> ST.get

  heading  <- heading (fromIntegral level) . _lineBreakLength <$> ST.get
  let tag = makeTag (fromMaybe headerText tagText) fileName

  case fromIntegral level of
    1 -> do
      indentLevel += 1
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
  convertInfo <- ST.get
  let justify x = if convertInfo ^. breakText
        then wrapText LeftA (fromIntegral $ convertInfo ^. lineBreakLength) x
        else x
  text' <- justify . T.unwords . ([text] ++) <$> nodesToVimHelp conts
  (text' :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ PARAGRAPH conts) : xs) = do
  text <- T.unlines <$> nodesToVimHelp conts
  (text :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ BLOCK_QUOTE conts) : xs) = do
  text <- T.unlines . map (("│ " <>) . T.strip) <$> nodesToVimHelp conts
  (text :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (CODE_BLOCK info conts) _) : xs) = do
  (("`" <> T.strip conts <> "`") :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (CODE text) conts) : xs) = do
  text' <- ("`" <>) . (<> "`") . T.unwords . ([text] ++) <$> nodesToVimHelp conts
  (text' :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (LIST (ListAttributes BULLET_LIST listTight _ _ )) conts): xs) = do
  text' <- T.unlines . (if listTight then id else intersperse "") . fmap ("- " <>) <$> nodesToVimHelp conts
  (text':) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (LIST (ListAttributes ORDERED_LIST listTight _ _ )) conts): xs) = do
  text' <- T.unlines . (if listTight then id else intersperse "") . zipWith (<>) ( map ((<> ". ") . T.pack . show) [1..]) <$> nodesToVimHelp conts
  (text':) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ ITEM conts): xs) = do 
  text' <- T.unwords . fmap T.strip <$> nodesToVimHelp conts
  (text':) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ EMPH conts): xs) = do 
  unic <- _unicode <$> ST.get
  text <- (if unic then italicize else id) . T.unwords <$> nodesToVimHelp conts
  
  undefined


nodesToVimHelp ((Node _ t conts) : xs) =
  ((T.pack $ show $ Node Nothing t conts) :) <$> nodesToVimHelp xs


nodesToVimHelp [] = return []


nodeToVimHelp :: MDConvertInfo -> Node -> (T.Text, T.Text)
nodeToVimHelp convertInfo (Node _ DOCUMENT (x@(Node _ (HEADING 1) [Node _ (TEXT title) _]) : x2@(Node _ PARAGRAPH [Node _ (TEXT tagline) _]) : xs))
  = (***) (T.unlines . ((header :) . (<> ["\nvim:tw=78:ts=8:ft=help:norl:"])))
          (T.unlines . _tags)
    $ ST.runState (nodesToVimHelp nodes) convertInfo
 where
  sameNamed = ((==) `on` dropExtension) (T.unpack title) (convertInfo ^. fileName)
  nodes     = if sameNamed then xs else x : x2 : xs
  header    = if sameNamed
    then docHeader (T.pack $ convertInfo ^. fileName)
                   tagline
                   (convertInfo ^. lineBreakLength)
    else
      align [T.pack $ convertInfo ^. fileName, convertInfo ^. tagLine]
            (convertInfo ^. lineBreakLength)
        <> "\n"

nodeToVimHelp convertInfo (Node _ DOCUMENT nodes) =
  T.unlines *** T.unlines . _tags $ ST.runState (nodesToVimHelp nodes) convertInfo

nodeToVimHelp _ node = (T.pack $ show node, "")


converter :: MDConvertInfo -> T.Text -> (T.Text, T.Text)
converter convertInfo = nodeToVimHelp convertInfo
  . commonmarkToNode [optSmart] [extStrikethrough, extTable, extTaskList]
