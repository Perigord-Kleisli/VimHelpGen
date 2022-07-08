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
import           Data.Traversable


log' :: Show a => a -> a
log' x = unsafePerformIO $ print x >> return x

logIf :: Show a => Bool -> a -> a
logIf True  x = log' x
logIf False x = x

logWith :: (Show a, Show b) => b -> a -> a
logWith b a = unsafePerformIO $ print a >> print b >> return a

logPass :: (Show a) => a -> b -> b
logPass a b = unsafePerformIO $ print a >> return b

nodesToVimHelp :: [Node] -> ST.State ConvertInfo [T.Text]

--nodesToVimHelp (x: (Node _ SOFTBREAK _): xs) =
  --(ix 0 %~)
  -- <$> ((<>) . (<> " ") . head <$> nodesToVimHelp [x]) -- Becomes equal to (<> (x <> " "))
  -- <*> nodesToVimHelp xs

--nodesToVimHelp ((Node _  _) : xs) = do
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

nodesToVimHelp (x : (Node _ SOFTBREAK _) : xs) =
  (ix 0 %~)
    <$> ((<>) . (<> " ") . head <$> nodesToVimHelp [x]) -- Becomes equal to (<> (x <> " "))
    <*> nodesToVimHelp xs

nodesToVimHelp ((Node _ (CODE text) conts) : xs) = do
  text <- T.unwords . (surround "`" text :) <$> nodesToVimHelp conts
  applyOnElse  (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ EMPH conts) : xs) = do
  unic <- _unicode <$> ST.get
  text <- applyIf italicize unic . T.unwords <$> nodesToVimHelp conts
  applyOnElse  (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ STRONG conts) : xs) = do
  unic <- _unicode <$> ST.get
  text <- applyIf boldize unic . T.unwords <$> nodesToVimHelp conts
  applyOnElse  (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (LINK url title) conts) : xs) = do
  let text' = case readLink url of
              HeaderLink text -> surround "|" text
              Footnote   text -> "TODO: handle footnotes" <> text
              URL        text -> text
  text <- (\x ->
            if T.null $ T.concat x
              then text'
              else T.unwords x <> ": (" <> text' <> ")" )
          <$> nodesToVimHelp conts

  (ix 0 %~ (text <>)) <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (HEADING level) conts) : xs) = do
  (headerText, tagText) <- case conts of
    [Node _ (TEXT text) conts, Node _ (HTML_INLINE tag) _] ->
      return (text, T.strip <$> (T.stripSuffix "-->" =<< T.stripPrefix "<!--" tag))

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

nodesToVimHelp ((Node (Just (PosInfo startLine startCol endLine endCol)) nodeType contents) : xs)
  = do
    lineLen   <- fromIntegral . _lineBreakLength <$> ST.get
    indentLvl <- _indentLevel <$> ST.get
    convertInfo <- ST.get

    let indented = indentLvl > 0

    text <- case nodeType of

      (TEXT text) -> T.strip . T.unwords . (text :) <$> nodesToVimHelp contents

      PARAGRAPH ->
        (surround "\n" `applyIf` not indented)
          .   wrapText LeftA lineLen
          .   T.unwords
          <$> nodesToVimHelp contents

      BLOCK_QUOTE ->
        do
          indentLevel += 1
          T.unlines . map ("│ " <>) . intersperse "" <$> nodesToVimHelp contents
        <* (indentLevel -= 1)

      CODE_BLOCK file conts ->
        mappend (T.unlines $ addToEnds [" >"] ["<"] $ map ("\t" <>) $ T.lines conts)
          .   const ""
          <$> nodesToVimHelp contents

      THEMATIC_BREAK ->
        mappend (T.replicate (fromIntegral lineLen) "=")
          .   T.unwords
          <$> nodesToVimHelp contents

      ITEM -> case contents of
        (x : xs) -> do
          T.concat <$> ST.liftM2
            (<>)
            (nodesToVimHelp [x])
            (fmap ("\n\t" <>) . concatMap T.lines <$> nodesToVimHelp xs)
        [] -> undefined

      (LIST (ListAttributes BULLET_LIST tight start delim)) ->
        do
          indentLevel += 1
          T.concat
            .   (intersperse "\n" `applyIf` not tight)
            .   tailMap ("\n" <>)
            .   fmap ("- " <>)
            <$> nodesToVimHelp contents
        <* (indentLevel -= 1)
      (LIST (ListAttributes ORDERED_LIST tight start delim)) ->
        do
          indentLevel += 1
          T.concat
            .   applyIf (intersperse "\n") (not tight)
            .   tailMap ("\n" <>)
            .   zipWith (<>) (map ((<> ". ") . showText) [start ..])
            <$> nodesToVimHelp contents
        <* (indentLevel -= 1)

      TABLE alignment -> return
                      $ T.unlines
                      $ borderize . transpose
                      $ map (respaceCol . fmap head . sequence)
                      $ transpose
                      $ map (flip zip alignment . tableToTextGrid convertInfo) contents

      TABLE_CELL -> T.unwords <$> nodesToVimHelp contents

      _ -> return $ showText $ Node Nothing nodeType contents

    (text :) <$> nodesToVimHelp xs


nodesToVimHelp []    = return []
nodesToVimHelp ((Node _ ntype conts):xs) = do
  (showText (ntype, conts):) <$> nodesToVimHelp xs

borderize :: [[T.Text]] -> [T.Text]
borderize table' = topBorder : intersperse midBorder (borderRow table') <> [bottomBorder]
   where
     borderRow = map (surround "│" . T.intercalate "│")
     (topBorder,midBorder,bottomBorder)
             = (\x -> (
                   "┌" <> T.intercalate "┬" x <> "┐"
                 , "├" <> T.intercalate "┼" x <> "┤"
                 , "└" <> T.intercalate "┴" x <> "┘"
                 ))
                 $ map ((`T.replicate` "─") . T.length)
                 $ head table'

respaceCol :: ([T.Text], TableCellAlignment) -> [T.Text]
respaceCol (texts, alignment) = map respaceCell texts
  where
    respaceCell :: T.Text -> T.Text
    respaceCell text = case alignment of
      NoAlignment    -> T.justifyLeft maxLenStr ' ' text
      LeftAligned    -> T.justifyLeft maxLenStr ' ' . T.stripStart $ text
      RightAligned   -> T.justifyRight maxLenStr ' ' . T.stripEnd $ text
      CenterAligned  -> T.center maxLenStr ' ' . T.strip  $ text
    maxLenStr = maximum $ map T.length texts

tableToTextGrid :: ConvertInfo -> Node -> [[T.Text]]
tableToTextGrid convertInfo (Node _ (TABLE alignment) conts) = transpose $ concatMap (tableToTextGrid convertInfo) conts
tableToTextGrid convertInfo (Node _ TABLE_ROW conts) = concatMap (tableToTextGrid convertInfo) conts
tableToTextGrid convertInfo (Node _ TABLE_CELL conts) = [fst $ ST.runState (nodesToVimHelp conts) convertInfo]
tableToTextGrid convertInfo node = [fst $ ST.runState (nodesToVimHelp [node]) convertInfo]

{-
 -drawTable :: [(TableCellAlignment, [T.Text])] -> T.Text
 -drawTable table = T.unlines transposedTable
 -  where
 -    transposedTable = borderize $ transpose $ map respaceCol $ transpose $ map (\(a,b) -> map (a,) b) table
 -
 -    borderize table' = topBorder : intersperse midBorder (borderRow table') <> [bottomBorder]
 -      where
 -        borderRow = map (surround "│" . T.intercalate "│")
 -        (topBorder,midBorder,bottomBorder)
 -                = (\x -> (
 -                      "┌" <> T.intercalate "┬" x <> "┐"
 -                    , "├" <> T.intercalate "┼" x <> "┤"
 -                    , "└" <> T.intercalate "┴" x <> "┘"
 -                    ))
 -                    $ map ((`T.replicate` "─") . T.length)
 -                    $ head table'
 -
 -    respaceCol :: [(TableCellAlignment, T.Text)] -> [T.Text]
 -    respaceCol collumn = map (respaceCell maxLenStr) collumn
 -      where
 -        maxLenStr = maximum $ map (T.length . snd) collumn
 -
 -    respaceCell :: Int ->  (TableCellAlignment,T.Text) -> T.Text
 -    respaceCell strSize (NoAlignment  ,text)  = T.justifyLeft strSize ' ' text
 -    respaceCell strSize (LeftAligned  ,text)  = T.justifyLeft strSize ' ' . T.stripStart $ text
 -    respaceCell strSize (RightAligned ,text)  = T.justifyRight strSize ' ' . T.stripEnd $ text
 -    respaceCell strSize (CenterAligned,text)  = T.center strSize ' ' . T.strip  $ text
 -}



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
  . commonmarkToNode [optUnsafe] [extTable, extTaskList]
