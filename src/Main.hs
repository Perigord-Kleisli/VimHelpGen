{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-} {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  ) where

import           Control.Lens

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified System.Console.Terminal.Size  as Term
import           System.Directory
import           System.FilePath.Posix


import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.List
import qualified Data.List.NonEmpty            as L
import           Data.Maybe
import           Numeric.Natural
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           Text.Printf
import           Text.Read


import           Data.Char
import qualified Markdown                      as M
import qualified VimHelpSyntax as V
import           Util


data Flags = Flags
  { _files      :: [FilePath]
  , _debug      :: Bool
  , _breakLen   :: Natural
  , _stdoutOnly :: Bool
  , _breakText  :: Bool
  , _unicode    :: Bool
  , _moduleName :: String
  , _tagLine    :: T.Text
  }
  deriving Show

defaultFlags :: Flags
defaultFlags = Flags { _files      = []
                     , _debug      = False
                     , _breakLen   = 0
                     , _stdoutOnly = False
                     , _breakText  = True
                     , _unicode    = False
                     , _moduleName = ""
                     , _tagLine    = ""
                     }
makeLenses ''Flags


options :: [OptDescr (Flags -> Flags)]
options = [
    Option ['m']
           ["module-name"]
           (ReqArg (moduleName .~) "MODULE NAME")
           "Name of the library module"
  , Option ['d'] ["debug"]   (NoArg (debug .~ True))   "Print debug info"
  , Option ['u'] ["unicode"] (NoArg (unicode .~ True)) "Enable Unicode Characters"
  , Option ['T']
           ["tagline"]
           (ReqArg ((tagLine .~) . T.pack) "TEXT")
           "Tagline of the module"
  , Option
    ['l']
    ["line-break-length"]
    (ReqArg
      (\x -> breakLen
        .~ fromMaybe (error $ printf "Invalid Number Provided '%s'" x) (readMaybe x)
      )
      "LENGTH"
    )
    "Length of line breaks (default: 120)"
  , Option [] ["stdout-only"] (NoArg (stdoutOnly .~ True)) "Send output to STDOUT only"
  ]

compileOpts :: [String] -> IO (Flags, [String])
compileOpts argv = do
  case getOpt RequireOrder options argv of
    (flags, args, []  ) -> return (foldl (flip id) defaultFlags flags, args)
    (_    , _   , errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: vimHelpGen [OPTIONS...] files...(default: 'README.md')"

unAbbr :: String -> String
unAbbr = \case
  ".md" -> "markdown"
  x     -> x

main :: IO ()
main = do

  readmeFile <- find ((== "README.MD") . fmap toUpper) <$> listDirectory "."
  flags <- do
    (flags, files') <- (second (<?> [fromMaybe "" readmeFile]) <$>) . compileOpts =<< getArgs

    -- Checks for a lua directory and chooses one of the available folders
    modName         <- if null $ flags ^. moduleName
      then
        (\xs -> choose (L.fromList xs) "Choose name of module: ")
          =<< (   listDirectory "."
              >>= maybe (return mempty) listDirectory
              .   ((== "lua") `find`)
              )
      else return []

    return -- For processing arguments after Parsing
      $ (files .~ files')
      . (breakLen %~ const 120 `applyOn` (== 0))
      . (moduleName %~ (<?> modName))
      $ flags

  forM_ (flags ^. files) $ \file -> do
    let fileType = unAbbr $ takeExtension file
    let convertInfo = V.ConvertInfo
          { V._lineBreakLength = flags ^. breakLen
          , V._indentLevel     = 0
          , V._tags            = []
          , V._fileName        = (`replaceExtension` "txt") (flags ^. moduleName)
          , V._moduleName      = T.pack $ flags ^. moduleName
          , V._breakText       = flags ^. breakText
          , V._tagLine         = flags ^. tagLine
          , V._unicode         = flags ^. unicode
          }

    let output (helpTxt, tags) = if flags ^. stdoutOnly
          then mapM_ T.putStrLn [helpTxt, tags]
          else do
            createDirectoryIfMissing True "doc/"
            T.writeFile (("doc/"++) $ (`replaceExtensions` "txt") $ flags ^. moduleName) helpTxt
            T.writeFile "doc/tags" tags

    case fileType of
      "markdown" -> T.readFile file >>= output . M.converter convertInfo
      _ -> ioError (userError $ "Unknown file type: '" ++ fileType ++ "'")
