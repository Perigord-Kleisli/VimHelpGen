{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  ) where

import           Lens.Micro.Platform
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified System.Console.Terminal.Size  as Term
import           System.Directory
import           System.FilePath.Posix


import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Data.List
import qualified Data.List.NonEmpty            as L
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           Text.Printf
import           Text.Read
import           Numeric.Natural


import qualified Markdown                      as M


data Flags = Flags
  { _files      :: [FilePath]
  , _fileType   :: String
  , _debug      :: Bool
  , _breakLen   :: Natural
  , _stdoutOnly :: Bool
  , _moduleName :: T.Text
  }
  deriving Show

defaultFlags :: Flags
defaultFlags = Flags { _files      = []
                     , _fileType   = ""
                     , _debug      = False
                     , _breakLen   = 0
                     , _stdoutOnly = False
                     , _moduleName = ""
                     }
makeLenses ''Flags

-- | Apply a function to a value given a condition
--
-- >>> map ((+5) `applyIf` (>5)) [2,3,4,5,7,4,6]
-- [2,3,4,5,12,4,11]
applyIf :: (a -> a) -> (a -> Bool) -> a -> a
applyIf f cond x = if cond x then f x else x

-- | Gets a specific percent of an `Int` using another `Int`
--
-- >>> 75 `percentOf` 231
-- 173
percentOf :: Int -> Int -> Int
percentOf x y = floor $ fromIntegral y * (fromIntegral x / 100.0)

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
(x : xs) !? 0 = Just x
(x : xs) !? n = xs !? (n - 1)

-- | Index an item from a list via User's choice
choose :: Show a => L.NonEmpty a -> String -> IO a
choose (x L.:| []) _ = return x
choose xs prompt = do
  mapM_ (putStrLn . (\(a, b) -> "[" ++ show a ++ "] " ++ show b))
    $ L.zip (L.fromList [0 ..]) xs
  putStr prompt
  choice <- readMaybe <$> getLine
  case choice of
    Nothing        -> errorState
    (Just choice') -> maybe errorState return $ L.toList xs !? choice'
  where errorState = choose xs "Invalid Choice, please try again: "

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option ['t']
           ["filetype", "ft"]
           (ReqArg (fileType .~ ) "FILETYPE")
           "File type"

  , Option ['m']
           ["module-name"]
           (ReqArg ((moduleName .~) . T.pack) "MODULE NAME")
           "Name of the library module"
  , Option ['d'] ["debug"] (NoArg (debug .~ True)) "Print debug info"

  , Option
      ['l']
      ["line-break-length"]
      (ReqArg (\x -> breakLen .~ fromMaybe (error $ printf "Invalid Number Provided '%s'" x) (readMaybe x)) "LENGTH")
      "Length of line breaks (default: 100)"

  , Option []
           ["stdout-only"]
           (NoArg (stdoutOnly .~ True))
           "Send output to STDOUT only"
  ]

compileOpts :: [String] -> IO (Flags, [String])
compileOpts argv = do
  case getOpt RequireOrder options argv of
    (flags, args, []) -> return (foldl (flip id) defaultFlags flags, args)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
 where
  header = "Usage: vimHelpGen [OPTIONS...] files...(default: 'README.md')"

unAbbr :: String -> String
unAbbr = \case
  ".md" -> "markdown"
  x     -> x

main :: IO ()
main = do

  flags <- do
    (flags, files') <- (second (<?> ["README.md"]) <$>) . compileOpts =<< getArgs
    termSize <- maybe 0 Term.width <$> Term.size

    -- Checks for a lua directory and chooses one of the available folders
    modName <- if T.null $ flags ^. moduleName 
        then return []
        else 
          (\case
            []  -> return mempty
            [x] -> return x
            xs  -> choose (L.fromList xs) "Choose name of module: "
          )
          =<< (   listDirectory "."
              >>= maybe (return mempty) listDirectory
              .   ((== "lua") `find`)
              )

    return -- For processing arguments after Parsing
      $ (files .~ files')
      . (fileType %~ (<?> (unAbbr $ takeExtension $ head files')))
      . (breakLen %~ const 100 `applyIf` (== 0))
      . (moduleName %~ (<?> T.pack modName))
      $ flags
  

  forM_ (flags ^. files) $ \file -> do

    let convertInfo = M.MDConvertInfo { 
                                      M._lineBreakLength = flags ^. breakLen
                                    , M._indentLevel     = 0
                                    , M._tags            = []
                                    , M._fileName        = file
                                    , M._moduleName      = flags ^. moduleName
                                    }

    let output (helpTxt, tags) = mapM_
          T.putStrLn
          [T.pack file, "-----", helpTxt, "tags:", "-----", tags]

    --hPrint stderr flags >> void getChar

    case flags ^. fileType of
      "markdown" -> T.readFile file >>= output . M.converter convertInfo
      _ ->
        ioError (userError $ "Unknown file type: '" ++ flags ^. fileType ++ "'")
