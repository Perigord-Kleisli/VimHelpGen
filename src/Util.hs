{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Util where

import GHC.Natural
import qualified Data.List.NonEmpty as L
import qualified Data.Text as T
import Data.Function
import Data.Char
import Text.Read
import Control.Arrow
import System.IO
import System.IO.Unsafe


-- | Apply a function to a value given a condition
--
-- >>> map ((+5) `applyIf` (>5)) [2,3,4,5,7,4,6]
-- [2,3,4,5,12,4,11]
applyOn :: (a -> a) -> (a -> Bool) -> a -> a
applyOn f cond x = if cond x then f x else x

applyOnElse :: (a -> b) -> (a -> b) -> (a -> Bool) -> a -> b
applyOnElse f g cond x = if cond x then f x else g x

-- | Apply a function to a value given a bool 
--
-- >>> applyIf (+4) True 1 
-- 5
-- >>> applyIf (+4) False 1 
-- 1
applyIf :: (a -> a) -> Bool -> a -> a
applyIf f cond x = if cond then f x else x

applyIfElse :: (a -> b) -> (a -> b) -> Bool -> a -> b
applyIfElse f g cond = if cond then f else g

-- | Gets a specific percent of an `Int` using another `Int`
--
-- >>> 75 `percentOf` 231
-- 173
percentOf :: Int -> Int -> Int
percentOf x y = floor @Double $ fromIntegral y * (fromIntegral x / 100.0)

-- | `<>` but doesnt append the second argument if first is not `mempty`
--
-- >>> "ab" <?> "AB"
-- "ab"
(<?>) :: (Monoid a, Eq a) => a -> a -> a
mX <?> mY | (mX, mY) == (mempty, mempty) = mempty
          | (mX, mY) == (mempty, mY)     = mY
          | otherwise                    = mX

-- | Safe version of (!!)
(!?) :: [a] -> Natural -> Maybe a
[]       !? _ = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)

prompt :: String -> IO String
prompt x = putStr x >> hFlush stdout >> getLine

-- | Index an item from a list via User's choice
choose :: Show a => L.NonEmpty a -> String -> IO a
choose (x L.:| [])  _ = return x
choose xs   promptTxt = do
  mapM_ (putStrLn . (\(a, b) -> "[" ++ show @Int a ++ "] " ++ show b))
    $ L.zip (L.fromList [0 ..]) xs
  promptChoice <- readMaybe <$> prompt promptTxt
  case promptChoice of
    Nothing        -> errorState
    (Just choice') -> maybe errorState return $ L.toList xs !? choice'
  where errorState = choose xs "Invalid Choice, please try again: "


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

strikethrough :: T.Text -> T.Text
strikethrough = (`T.snoc` toEnum 822) . T.intersperse (toEnum 822)

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
wrapText alignment size = T.intercalate "\n" . filter (/="") . alignFunc . truncateLines size . smartChunksOf size
  where
    alignFunc = case alignment of
      LeftA -> map T.stripStart
      CenterA -> map ((`align` size) . (\x -> ["",x,""]) . T.stripEnd)
      RightA -> map ((`align` size) . (\x -> ["",x]) . T.stripEnd)
      JustifiedA -> map ((`align` size) . T.words)

wrapWithIndent :: AlignMethod -> Natural -> T.Text -> T.Text -> T.Text
wrapWithIndent alignment size' indent = T.intercalate "\n" . map (indent <>) . filter (/="") . alignFunc . truncateLines size . smartChunksOf size
  where
    size = fromIntegral $ max 0 $ fromIntegral size' - T.length indent
    alignFunc = case alignment of
      LeftA -> map T.stripStart
      CenterA -> map ((`align` size) . (\x -> ["",x,""]) . T.stripEnd)
      RightA -> map ((`align` size) . (\x -> ["",x]) . T.stripEnd)
      JustifiedA -> map ((`align` size) . T.words)

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
align = (`alignWith` ' ')

alignWith :: [T.Text] -> Char -> Natural -> T.Text
alignWith [] _ _ = ""
alignWith [x] (T.singleton -> c) n = let [leftSpace, rightSpace] = divSpread (fromIntegral n - T.length x) 2
                                      in T.replicate leftSpace c <> x <> T.replicate rightSpace c
alignWith [x,y] (T.singleton -> c) n = x <> T.replicate (fromIntegral n - T.length x - T.length y) c <> y
alignWith xs (T.singleton -> c) n = mconcat $ longZipWith (<>) xs spaces
  where
    spaces = map (`T.replicate` c) $ tail $ divSpread (fromIntegral n - sum (map T.length xs)) $ length xs

-- | Calls `align` but takes only characters fitting the provided text length
cuttedAlign :: [T.Text] -> Natural -> T.Text
cuttedAlign text len = T.take (fromIntegral len) $ align text len

surround :: Monoid a => a -> a -> a
surround c = (<> c) . (c <>)

addToEnds :: Monoid a => a -> a -> a -> a
addToEnds a b = (a <>) . (<> b)

showText :: Show a => a -> T.Text
showText = T.pack . show

tailMap :: (a -> a) -> [a] -> [a]
tailMap f (x:xs) = x : map f xs
tailMap _ [] = []

log' :: Show a => a -> a
log' x = unsafePerformIO $ print x >> return x

logIf :: Show a => Bool -> a -> a
logIf True  x = log' x
logIf False x = x

logWith :: (Show a, Show b) => b -> a -> a
logWith b a = unsafePerformIO $ print a >> print b >> return a

logPass :: (Show a) => a -> b -> b
logPass a b = unsafePerformIO $ print a >> return b


