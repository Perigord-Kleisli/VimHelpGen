{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module VimHelpSyntax where

import qualified Data.Text as T
import Control.Lens
import Numeric.Natural
import Util
import Control.Applicative
import Data.Maybe
import Control.Arrow
import Data.Function


data ConvertInfo = ConvertInfo
  { _lineBreakLength :: Natural
  , _indentLevel     :: Natural
  , _tags            :: [(T.Text, T.Text)]
  , _noTOC           :: Bool
  , _tagLine         :: T.Text
  , _unicode         :: Bool
  , _fileName        :: FilePath
  , _breakText       :: Bool
  , _moduleName      :: T.Text
  , _nested          :: Bool
  }
  deriving Show
makeLenses ''ConvertInfo

data Link = HeaderLink T.Text | URL T.Text | Footnote T.Text
  deriving Show

readLink :: T.Text -> Link
readLink text = fromMaybe undefined $
      HeaderLink <$> T.stripPrefix "#" text
  <|> Footnote <$> T.stripPrefix "^" text
  <|> Just (URL text)

tag :: T.Text -> T.Text
tag = ("*" <>) . (<> "*") . T.strip . T.replace " " "-"

makeTag :: T.Text -> T.Text -> T.Text -> T.Text
makeTag docFileName titleName tagName =
  T.intercalate "\t\t\t" [titleName, docFileName , mconcat ["/*", tagName, "*"]]

docHeader :: T.Text -- ^ title
  -> T.Text -- ^ tagline
  -> Natural -- ^ length of pageBreak
  -> T.Text
docHeader title tagline breakLen = T.unlines
  [align [title, tagline] breakLen,
   mempty,
   pageBreak breakLen]

makeToc :: Natural -> T.Text -> [(T.Text, T.Text)] -> T.Text
makeToc lineLen tagname 
               = T.unlines
               . mappend [heading 1 lineLen tagname $ Just (T.toUpper tagname <> "-CONTENTS")]
               . map ( (\x -> alignWith x ' ' lineLen)
                     . uncurry (mappend @[T.Text] `on` pure)
                     . second (surround "|"))
               . zipWith ( first . mappend . (<>". ") . showText @Int) [1..]

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

heading 3 breakLen title tagName =
  align [title, maybe (tag title) tag tagName] breakLen

heading n breakLen title tagName =
  align [T.replicate (fromIntegral n - 3) "    " <> title, maybe (tag title) tag tagName] breakLen
