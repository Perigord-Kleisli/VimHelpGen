{-# LANGUAGE OverloadedStrings #-}

module VimHelpSyntax
  (pageBreak,
   makeTag,
   heading,
   tag,
   divSpread,
   align,
   docHeader)
  where

import qualified Data.Text as T
import Numeric.Natural
import Text.Printf
import Control.Arrow

-- | Zips 2 lists of equal type, if one list falls short then the remaining items of the longer lists will be appended
--
-- >>> longZipWith (+) [2,4,2,1,5] [3,2]
-- [5,6,2,1,5]
longZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
longZipWith _ [] ys = ys
longZipWith _ xs [] = xs
longZipWith f (x:xs) (y:ys) = f x y : longZipWith f xs ys

-- | Divides together 2 Ints and returns an array where if added would return the dividend
--
-- >>> divSpread 37 10
-- [4,4,4,4,4,4,4,3,3,3]
divSpread :: Int -> Int -> [Int]
divSpread x y = uncurry (longZipWith (+))
              $ ((y `replicate`) . abs) *** ((`replicate` 1) . abs)
              $ divMod x y

-- | Distributes text in a list in such a way they would be perfectly spaced and aligned within a certain length
--
-- >>> align ["e", "d"] 23
-- "e                     d"
-- >>> align ["e"] 23
-- "           e           "
-- >>> align ["e","sd","gf","ds"] 23
-- "e    sd    gf    ds"
align :: [T.Text] -> Natural -> T.Text
align [x] n = let [leftSpace, rightSpace] = divSpread (fromIntegral n - T.length x) 2
              in T.replicate leftSpace " " <> x <> T.replicate rightSpace " "
align [x,y] n = x <> T.replicate (fromIntegral n - T.length x - T.length y) " " <> y
align xs n = mconcat $ longZipWith (<>) xs spaces
  where
    spaces = map (`T.replicate` " ") $ tail $ divSpread (fromIntegral n - sum (map T.length xs)) $ length xs

-- | Calls `align` but takes only characters fitting the provided text length
cuttedAlign :: [T.Text] -> Natural -> T.Text
cuttedAlign text len = T.take (fromIntegral len) $ align text len

tag :: T.Text -> T.Text
tag = ("*" <>) . (<> "*") . T.strip


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
