{-# LANGUAGE OverloadedStrings #-}

module VimHelpSyntax
  (pageBreak,
   makeTag,
   heading,
   tag,
   divSpread,
   splitWhen,
   truncateLines,
   splitBefore,
   wrapText,
   AlignMethod(..),
   align,
   smartChunksOf,
   italicize,
   boldize,
   docHeader)
  where

import qualified Data.Text as T
import Numeric.Natural
import Text.Printf
import Control.Arrow
import Data.Function
import Data.Char

breakListBefore :: ([a] -> Bool) -> [a] -> ([a],[a])
breakListBefore = go []
  where
    go accum _ [] = (accum, [])
    go accum f (x:xs) 
       | f (accum <> [x]) = (accum,x:xs)
       | otherwise = go (accum <> [x]) f xs

-- | Splits a list via the condition of an accumulation
--
-- >>> splitWhen ((5 <=) . foldr ((+) . T.length) 0) $ T.words "12 345 123 45 12345 1 2 3 4 5 123456 789"
-- [["12","345"],["123","45"],["12345"],["1","2","3","4","5"],["123456"],["789"]]
splitWhen :: ([a] -> Bool) -> [a] -> [[a]]
splitWhen = go [] 
  where 
    go :: [a] -> ([a] -> Bool) -> [a] -> [[a]]
    go [] _ [] = []
    go x _ [] = [x]
    go accum f (x:xs)
      | f (accum <> [x]) = accum <> [x] : go [] f xs
      | otherwise = go (accum <> [x]) f xs

-- | Like `splitWhen` but splits before the predicate becomes true whenever possible 
--  
-- >>> splitBefore  ((5 <=) . foldr ((+) . T.length) 0) $ T.words "12 345 123 45 12345 1 2 3 4 5 123456 789"
-- [["12"],["345"],["123"],["45"],["12345"],["1","2","3","4"],["5"],["123456"],["789"]]
splitBefore :: ([a] -> Bool) -> [a] -> [[a]]
splitBefore f xs' = case breakListBefore f xs' of
  ([],[]) -> []
  (l,[]) -> [l]
  ([],x:xs) -> [x] : splitBefore f xs
  (l,r) -> l : splitBefore f r

-- | Splits text into chunks of size N or lower, respecting words and spaces whenever possible
--  
-- >>> map T.concat $ smartChunksOf 20 "asd asd asd asdkalsjkldj askdlj askd asd dsdsd"
-- ["asd asd asd ","asdkalsjkldj askdlj ","askd asd dsdsd"]
smartChunksOf :: Natural -> T.Text -> [T.Text]
smartChunksOf size text = map T.concat
                        $ splitBefore ((fromIntegral size <) . foldr ((+) . T.length) 0 ) 
                        $ T.groupBy ((==) `on` isSpace) text

italicize :: T.Text -> T.Text
italicize = T.map italic

italic :: Char -> Char
italic c 
  | isAsciiUpper c = toEnum $ fromEnum c + 119795
  | isAsciiLower c = toEnum $ fromEnum c + 120257
  | otherwise = c

boldize :: T.Text -> T.Text
boldize = T.map bold

bold :: Char -> Char
bold c 
  | isAsciiUpper c = toEnum $ fromEnum c + 119743
  | isAsciiLower c = toEnum $ fromEnum c + 119737
  | otherwise = c


-- | Arranges characters in a list of text such that all elements will have equal length except for the last, it will also add "-" to truncated words
-- \
-- >>> truncateLines 5 ["Lorrem","Ipsumm"," ","Dolores"," Sit ","Amet"]
-- ["Lorr-","emIp-","summ ","Dolo-","res S-","it Amet"]
truncateLines :: Natural -> [T.Text] -> [T.Text]
truncateLines _ [] = []
truncateLines size [x] = let 
        size' = if isSpace $ T.last x then size else size - 1
        (l,r) = T.splitAt (fromIntegral size') x 
        l' = if not $ isSpace $ T.last l then l else l <> "-"
        in l' : [r]

truncateLines size (x:y:xs) = if fromIntegral size < T.length x 
  then let 
        size' = if isSpace $ T.last x then size else size - 1
        (l,r) = T.splitAt (fromIntegral size') x 
        l' = if isSpace $ T.last l then l else l <> "-"
        in l' : truncateLines size (r<>y:xs)
  else x : truncateLines size (y:xs)

data AlignMethod = LeftA | CenterA | RightA | JustifiedA
  deriving (Show, Read)

-- | Aligns text upon a given size
wrapText :: AlignMethod -> Natural -> T.Text -> T.Text
--wrapText alignment size = T.intercalate "\n" . filter (not . T.all isSpace) . alignFunc . truncateLines size . smartChunksOf size
wrapText alignment size = T.intercalate "\n" . filter (/="") . alignFunc . truncateLines size . smartChunksOf size
  where 
    alignFunc = case alignment of
      LeftA -> map T.stripStart
      CenterA -> map ((`align` size) . (\x -> ["",x,""]) . T.stripEnd)
      RightA -> map ((`align` size) . (\x -> ["",x]) . T.stripEnd)
      JustifiedA -> map ((`align` size) . T.words)
--wrapText LeftA n =     T.intercalate "\n" . map T.strip . truncateLines n . smartChunksOf n 
--wrapText RightA n =    T.intercalate "\n" . map ((`align` n) . (\x -> ["",x]) .  T.strip) . truncateLines n . smartChunksOf n 
--wrapText CenterA n =   T.intercalate "\n" . map ((`align` n) . (\x -> ["",x,""]) .  T.strip) . truncateLines n . smartChunksOf n 
--wrapText Justified n = T.intercalate "\n" . map ((`align` n) . T.words) . truncateLines n . smartChunksOf n 

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
align [] _ = ""
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
