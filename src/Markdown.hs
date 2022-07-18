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

import           Data.Function

import           Data.List
import           Data.Maybe
import           VimHelpSyntax

import           Util


nodesToVimHelp :: [Node] -> ST.State ConvertInfo [T.Text]

nodesToVimHelp (x : (Node _ SOFTBREAK _) : xs) =
  (ix 0 %~)
    <$> ((<>) . (<> " ") . head <$> nodesToVimHelp [x]) -- Becomes equal to (<> (x <> " "))
    <*> nodesToVimHelp xs

nodesToVimHelp ((Node _ (CODE codeText) conts) : xs) = do
  text <- T.unwords . (surround "`" codeText :) <$> nodesToVimHelp conts
  applyOnElse (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ EMPH conts) : xs) = do
  unic <- _unicode <$> ST.get
  text <- applyIf italicize unic . T.unwords <$> nodesToVimHelp conts
  applyOnElse (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (HTML_INLINE text') conts) : xs) = do
  text <- (text' <>) . T.unwords <$> nodesToVimHelp conts
  applyOnElse (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ STRONG conts) : xs) = do
  unic <- _unicode <$> ST.get
  text <- applyIf boldize unic . T.unwords <$> nodesToVimHelp conts
  applyOnElse (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ STRIKETHROUGH conts) : xs) = do
  unic <- _unicode <$> ST.get
  text <- applyIf strikethrough unic . T.unwords <$> nodesToVimHelp conts
  applyOnElse (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (IMAGE url _) conts) : xs) = do
  text <- (\x -> if T.null $ T.concat x then url else T.unwords x <> ": (" <> url <> ")")
    <$> nodesToVimHelp conts
  applyOnElse (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (LINK url _) conts) : xs) = do
  let text' = case readLink url of
        HeaderLink text -> surround "|" text
        Footnote   text -> "TODO: handle footnotes" <> text
        URL        text -> text
  text <-
    (\x -> if T.null $ T.concat x then text' else T.unwords x <> ": (" <> text' <> ")")
      <$> nodesToVimHelp conts

  applyOnElse (text :) (ix 0 %~ (text <>)) null <$> nodesToVimHelp xs

nodesToVimHelp ((Node _ (HEADING level) conts) : xs) = do
  (headerText, tagText) <- case conts of
    [Node _ (TEXT text) _, Node _ (HTML_INLINE htmlTag) _] ->
      return (text, T.strip <$> (T.stripSuffix "-->" =<< T.stripPrefix "<!--" htmlTag))

    [] -> return ("", Nothing)

    x  -> (, Nothing) . T.unwords <$> nodesToVimHelp x

  --fileName <- T.pack . _fileName <$> ST.get
  header <- heading (fromIntegral level) . _lineBreakLength <$> ST.get
  let tagName = fromMaybe headerText tagText

  text <- if level <= 3
    then do
      tags %= (++ [(headerText, tagName)])
      indentLevel .= fromIntegral (max 0 (level - 2))
      return $ header headerText tagText
    else do
      indentLevel .= fromIntegral (max 0 (level - 2))
      return $ header headerText Nothing

  (text :) <$> nodesToVimHelp xs

nodesToVimHelp ((Node (Just (PosInfo{})) nodeType contents) : xs) = do
  convertInfo <- ST.get
  lineLen     <- fromIntegral . _lineBreakLength <$> ST.get
  isNested    <- _nested <$> ST.get
  let defIndent = "    "
  indent <-
    applyIf (const "") isNested
    .   (`T.replicate` defIndent)
    .   fromIntegral
    .   _indentLevel
    <$> ST.get

  text <- case nodeType of

    (TEXT text) -> T.stripEnd . T.unwords . (text :) <$> nodesToVimHelp contents

    PARAGRAPH ->
      (surround "\n" `applyIf` not isNested)
        .   wrapWithIndent LeftA lineLen indent
        .   T.unwords
        <$> nodesToVimHelp contents

    BLOCK_QUOTE ->
      do
        nested .= True
        T.unlines . map ("│ " <>) . intersperse "" <$> nodesToVimHelp contents
      <* (nested .= False)

    CODE_BLOCK _ conts ->
      mappend
          ( (if isNested
              then surround "`" . T.unwords
              else T.unlines . addToEnds [" >"] ["<"]
            )
          $ map ((indent <> defIndent) <>)
          $ T.lines conts
          )
        .   const ""
        <$> nodesToVimHelp contents

    THEMATIC_BREAK ->
      mappend (T.replicate (fromIntegral lineLen) "=")
        .   T.unwords
        <$> nodesToVimHelp contents

    ITEM -> case contents of
      (x : items) -> do
        T.concat <$> ST.liftM2
          (<>)
          (nodesToVimHelp [x])
          (fmap (("\n" <> defIndent) <>) . concatMap T.lines <$> nodesToVimHelp items)
      [] -> undefined

    (LIST (ListAttributes BULLET_LIST tight _ _)) ->
      do
        nested .= True
        ((<> "\n") `applyIf` not isNested)
          .   T.concat
          .   (intersperse "\n" `applyIf` not tight)
          .   tailMap ("\n" <>)
          .   fmap ((indent <> defIndent <> "- ") <>)
          <$> nodesToVimHelp contents
      <* (nested .= False)
    (LIST (ListAttributes ORDERED_LIST tight start _)) ->
      do
        nested .= True
        ((<> "\n") `applyIf` not isNested)
          .   T.concat
          .   applyIf (intersperse "\n") (not tight)
          .   tailMap ("\n" <>)
          .   zipWith (<>) (map ((<> ". ") . showText) [start ..])
          <$> nodesToVimHelp contents
      <* (nested .= False)

    TABLE alignment ->
      do
        nested .= True
        return
          $ T.unlines
          $ borderize
          $ transpose
          $ map (respaceCol . fmap head . sequence)
          $ transpose
          $ map (flip zip alignment . tableToTextGrid convertInfo) contents
      <* (nested .= False)

    _ -> return $ showText $ Node Nothing nodeType contents

  (text :) <$> nodesToVimHelp xs


nodesToVimHelp []                          = return []
nodesToVimHelp ((Node _ ntype conts) : xs) = do
  (showText (ntype, conts) :) <$> nodesToVimHelp xs

borderize :: [[T.Text]] -> [T.Text]
borderize table' = topBorder : intersperse midBorder (borderRow table') <> [bottomBorder]
 where
  borderRow = map (surround "│" . T.intercalate "│")
  (topBorder, midBorder, bottomBorder) =
    (\x ->
        ( "┌" <> T.intercalate "┬" x <> "┐"
        , "├" <> T.intercalate "┼" x <> "┤"
        , "└" <> T.intercalate "┴" x <> "┘"
        )
      )
      $ map ((`T.replicate` "─") . T.length)
      $ head table'

respaceCol :: ([T.Text], TableCellAlignment) -> [T.Text]
respaceCol (texts, alignment) = map respaceCell texts
 where
  respaceCell :: T.Text -> T.Text
  respaceCell text = case alignment of
    NoAlignment   -> T.justifyLeft maxLenStr ' ' text
    LeftAligned   -> T.justifyLeft maxLenStr ' ' . T.stripStart $ text
    RightAligned  -> T.justifyRight maxLenStr ' ' . T.stripEnd $ text
    CenterAligned -> T.center maxLenStr ' ' . T.strip $ text
  maxLenStr = maximum $ map T.length texts

tableToTextGrid :: ConvertInfo -> Node -> [[T.Text]]
tableToTextGrid convertInfo (Node _ (TABLE _) conts) =
  transpose $ concatMap (tableToTextGrid convertInfo) conts
tableToTextGrid convertInfo (Node _ TABLE_ROW conts) =
  concatMap (tableToTextGrid convertInfo) conts
tableToTextGrid convertInfo (Node _ TABLE_CELL conts) =
  [applyOn (const [" "]) null $ fst $ ST.runState (nodesToVimHelp conts) convertInfo]
tableToTextGrid convertInfo node =
  [fst $ ST.runState (nodesToVimHelp [node]) convertInfo]

tagsToTxt :: T.Text -> [(T.Text, T.Text)] -> [T.Text]
tagsToTxt filename = map (uncurry $ makeTag filename)

nodeToVimHelp :: ConvertInfo -> Node -> (T.Text, T.Text)
nodeToVimHelp convertInfo (Node _ DOCUMENT (x1@(Node _ (HEADING 1) [Node _ (TEXT title) _]) : x2@(Node _ PARAGRAPH [Node _ (TEXT tagline) _]) : xs))
  = ( T.unlines $ [header, toc] `addToEnds` ["\nvim:tw=78:ts=8:ft=help:norl:"] $ text
    , T.unlines $ tagsToTxt (convertInfo' ^. moduleName) (convertInfo' ^. tags)
    )
 where
  (text, convertInfo') = ST.runState (nodesToVimHelp nodes) convertInfo
  sameNamed = ((==) `on` dropExtension) (T.unpack title) (convertInfo ^. fileName)
  nodes                = (\a -> x1 : x2 : a) `applyIf` not sameNamed $ xs

  header               = if sameNamed
    then docHeader (T.pack $ convertInfo ^. fileName)
                   tagline
                   (convertInfo ^. lineBreakLength)
    else
      align [T.pack $ convertInfo ^. fileName, convertInfo ^. tagLine]
            (convertInfo ^. lineBreakLength)
        <> "\n"

  toc = if convertInfo' ^. noTOC
    then ""
    else makeToc (convertInfo' ^. lineBreakLength)
                 (convertInfo' ^. moduleName)
                 (convertInfo' ^. tags)




nodeToVimHelp convertInfo (Node _ DOCUMENT nodes) =
  ( T.unlines $ [header, toc] `addToEnds` ["\nvim:tw=78:ts=8:ft=help:norl:"] $ text
  , T.unlines $ tagsToTxt (convertInfo' ^. moduleName) (convertInfo' ^. tags)
  )
 where
  (text, convertInfo') = ST.runState (nodesToVimHelp nodes) convertInfo
  header =
    align [T.pack $ convertInfo ^. fileName, convertInfo ^. tagLine]
          (convertInfo ^. lineBreakLength)
      <> "\n"
  toc = if convertInfo' ^. noTOC
    then ""
    else makeToc (convertInfo' ^. lineBreakLength)
                 (convertInfo' ^. moduleName)
                 (convertInfo' ^. tags)

nodeToVimHelp _ _ = undefined

converter :: ConvertInfo -> T.Text -> (T.Text, T.Text)
converter convertInfo = nodeToVimHelp convertInfo
  . commonmarkToNode [] [extTable, extTaskList, extStrikethrough]
