{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module VimHelpSyntax where

import qualified Data.Text as T
import Control.Lens
import Numeric.Natural
import Text.Printf
import Control.Arrow
import Data.Function
import Data.Char
import Util


data ConvertInfo = ConvertInfo
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
makeLenses ''ConvertInfo

tag :: T.Text -> T.Text
tag = ("*" <>) . (<> "*") . T.strip . T.replace " " "_"

makeTag :: T.Text -> T.Text -> T.Text
makeTag tagName fileName =
  T.intercalate "\t" [tagName, fileName, mconcat ["/*", tagName, "*"]]

docHeader :: T.Text -- ^ title
  -> T.Text -- ^ tagline
  -> Natural -- ^ length of pageBreak
  -> T.Text
docHeader title tagline breakLen = T.unlines
  [align [title, tagline] breakLen,
   mempty,
   pageBreak breakLen]

pageBreak :: Natural -> T.Text
pageBreak = (`T.replicate` "=") . fromIntegral

heading :: Natural -- ^ Level of Header
  -> Natural -- ^ Length of pageBreak
  -> T.Text -- ^ Header Title
  -> Maybe T.Text -- ^ Optional Tags
  -> T.Text

heading 1 breakLen title tagName = T.unlines
  [pageBreak breakLen,
   align [title, maybe (tag title) tag tagName] breakLen]

heading 2 breakLen title tagName = T.unlines 
  [T.replicate (fromIntegral breakLen) "-",
   align [title, maybe (tag title) tag tagName] breakLen]

heading n breakLen title tagName = 
     T.replicate (pred $ fromIntegral n) "\t" 
  <> align [title, maybe (tag title) tag tagName] breakLen
