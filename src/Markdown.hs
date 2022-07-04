{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Markdown
  ( converter
  ) where

import           CMarkGFM
import           Control.Lens
import qualified Control.Monad.State.Lazy      as ST
import qualified Data.Text                     as T
import           System.FilePath.Posix

import           Control.Arrow
import           Data.Bits                      ( Bits(xor) )
import           Data.Char
import           Data.Function
import           GHC.Read
import           Text.Printf
import           Text.Read

import           Data.List
import           Data.Maybe
import           GHC.IO
import           Numeric.Natural
import           VimHelpSyntax

import           Util


log' :: Show a => a -> a
log' x = unsafePerformIO $ print x >> return x

nodesToVimHelp :: [Node] -> ST.State ConvertInfo [T.Text]

--nodesToVimHelp (x: (Node _ SOFTBREAK _): xs) =
  --(ix 0 %~)
  -- <$> ((<>) . (<> " ") . head <$> nodesToVimHelp [x]) -- Becomes equal to (<> (x <> " "))
  -- <*> nodesToVimHelp xs

--nodesToVimHelp ((Node _ THEMATIC_BREAK _) : xs) = do
  --x <- ST.get <&> (`T.replicate` "=") . fromIntegral . _lineBreakLength
  --(x :) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ (HEADING level) conts) : xs) = do
  --(headerText, tagText) <- case conts of
    --[] -> return ("", Nothing)
    --[Node _ (TEXT text) conts, Node _ (HTML_INLINE tag) _] ->
      --return (text, T.strip <$> (T.stripSuffix "-->" =<< T.stripPrefix "<!--" tag))
    --x -> (, Nothing) . T.unwords <$> nodesToVimHelp x


  --fileName <- T.pack . _fileName <$> ST.get

  --heading  <- heading (fromIntegral level) . _lineBreakLength <$> ST.get
  --let tag = makeTag (fromMaybe headerText tagText) fileName

  --case fromIntegral level of
    --1 -> do
      --indentLevel += 1
      --tags %= (++ [tag])
      --(heading headerText tagText :) <$> nodesToVimHelp xs
    --2 -> do
      --indentLevel .= 2
      --tags %= (++ [tag])
      --(heading headerText tagText :) <$> nodesToVimHelp xs
    --x -> do
      --indentLevel .= x
      --(heading headerText Nothing :) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ (TEXT text) conts) : xs) = (text: ) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ PARAGRAPH conts) : xs) = do
  --lineLen <- fromIntegral . _lineBreakLength <$> ST.get
  --justify <- applyIf (wrapText LeftA lineLen) . _breakText <$> ST.get
  --text <- justify . T.unwords <$> nodesToVimHelp conts
  --(text :) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ BLOCK_QUOTE conts) : xs) = do
  --text <- T.unlines . map (("│ " <>) . T.strip) <$> nodesToVimHelp conts
  --(text :) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ (LIST (ListAttributes BULLET_LIST listTight _ _ )) conts): xs) = do
  --text' <- T.unlines . (if listTight then id else intersperse "") . fmap ("- " <>) <$> nodesToVimHelp conts
  --(text':) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ (LIST (ListAttributes ORDERED_LIST listTight listStart _ )) conts): xs) = do
  --text' <- T.unlines . applyIf (intersperse "") (not listTight) . zipWith (<>) ( map ((<> ". ") . T.pack . show) [listStart..]) <$> nodesToVimHelp conts
  --(text':) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ ITEM conts): xs) = do
  --text' <- T.unwords . fmap T.strip <$> nodesToVimHelp conts
  --(text':) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ (CODE text) conts) : xs) = do
  --text' <- ("`" <>) . (<> "`") . T.unwords . ([text] ++) <$> nodesToVimHelp conts
  --(ix 0 %~ (text' <>)) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ (CODE_BLOCK info conts) _) : xs) = do
  --let text = T.unlines $ map ("\t"<>) $ T.lines conts
  --(addToEnds ">\n" "<" text:) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ EMPH conts): xs) = do
  --unic <- _unicode <$> ST.get
  --text <- applyIf italicize unic . T.unwords <$> nodesToVimHelp conts
  --(ix 0 %~ (text <>)) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ STRONG conts): xs) = do
  --unic <- _unicode <$> ST.get
  --text <- applyIf boldize unic . T.unwords <$> nodesToVimHelp conts
  --(ix 0 %~ (text <>)) <$> nodesToVimHelp xs


nodesToVimHelp ((Node _ (CODE text) conts): xs) = do
  text <- T.unwords . (surround "`" text:) <$> nodesToVimHelp conts
  (ix 0 %~ (text <>)) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ EMPH conts): xs) = do
  unic <- _unicode <$> ST.get
  text <- applyIf italicize unic . T.unwords <$> nodesToVimHelp conts
  (ix 0 %~ (text <>)) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ STRONG conts): xs) = do
  unic <- _unicode <$> ST.get
  text <- applyIf boldize unic . T.unwords <$> nodesToVimHelp conts
  (ix 0 %~ (text <>)) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (HEADING level) conts) : xs) = do
  (headerText, tagText) <- case conts of
    [Node _ (TEXT text) conts, Node _ (HTML_INLINE tag) _] ->
      return ( text
             , T.strip <$> (T.stripSuffix "-->" =<< T.stripPrefix "<!--" tag))

    [] -> return ("", Nothing)

    x  -> (, Nothing) . T.unwords <$> nodesToVimHelp x

  fileName <- T.pack . _fileName <$> ST.get
  header   <- heading (fromIntegral level) . _lineBreakLength <$> ST.get
  let tag = makeTag (fromMaybe headerText tagText) fileName

  text <- case fromIntegral level of
    1 -> do
      tags %= (++ [tag])
      return $ header headerText tagText
    2 -> do
      tags %= (++ [tag])
      return $ header headerText tagText
    _ -> do
      return $ header headerText Nothing

  (text :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ nodeType contents): xs) = do 
  lineLen <- _lineBreakLength <$> ST.get
  text <- case nodeType of
    (TEXT text) -> T.unwords . (text :) <$> nodesToVimHelp contents 
    PARAGRAPH -> wrapText LeftA lineLen . T.unwords <$> nodesToVimHelp contents
    _ -> return $ showText $ Node Nothing nodeType contents

  (text: ) <$> nodesToVimHelp xs

--nodesToVimHelp ((Node _ t conts) : xs) =
  --((T.pack $ show $ Node Nothing t conts) :) <$> nodesToVimHelp xs

nodesToVimHelp [] = return []


nodeToVimHelp convertInfo (Node _ DOCUMENT (x1@(Node _ (HEADING 1) [Node _ (TEXT title) _]) : x2@(Node _ PARAGRAPH [Node _ (TEXT tagline) _]) : xs))
  = (***) (T.unlines . ((header :) . (<> ["\nvim:tw=78:ts=8:ft=help:norl:"])))
          (T.unlines . _tags)
    $ ST.runState (nodesToVimHelp nodes) convertInfo
 where
  sameNamed = ((==) `on` dropExtension) (T.unpack title) (convertInfo ^. fileName)
  nodes     = (\a -> x1 : x2 : a) `applyIf` not sameNamed $ xs
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



converter :: ConvertInfo -> T.Text -> (T.Text, T.Text)

converter convertInfo = nodeToVimHelp convertInfo
  . commonmarkToNode [optSmart] [extStrikethrough, extTable, extTaskList]
