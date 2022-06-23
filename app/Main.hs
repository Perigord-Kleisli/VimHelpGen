{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  ) where

import           Lens.Micro
import           Lens.Micro.TH

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
import           Data.Maybe
import           System.Console.GetOpt
import           System.Environment
import           Text.Printf
import           Text.Read

import qualified Markdown                      as M


data Flags = Flags
  { _files      :: [FilePath]
  , _fileType   :: String
  , _debug      :: Bool
  , _breakLen   :: Int
  , _indentType :: M.Indent
  , _stdoutOnly :: Bool
  , _moduleName :: T.Text
  }
  deriving Show

defaultFlags :: Flags
defaultFlags = Flags { _files      = []
                     , _fileType   = ""
                     , _debug      = False
                     , _breakLen   = -1
                     , _indentType = M.Spaces
                     , _stdoutOnly = False
                     , _moduleName = ""
                     }
makeLenses ''Flags

-- | Converts Bools to a common form of theirs in Lambda Calculus
-- boolComb True = (\x _ -> x)
-- boolComb False = (\_ x -> x)
boolComb :: Bool -> (a -> a -> a)
boolComb True  = const
boolComb False = const id

-- | Gets a specific percent of an `Int` using another `Int`
-- >>> 75 `percentOf` 231
-- 173
percentOf :: Int -> Int -> Int
percentOf x y = floor $ fromIntegral y * (fromIntegral x / 100.0)

unAbbr :: String -> String
unAbbr = \case
  ".md" -> "markdown"
  x    -> x

-- | `<>` but doesnt append the second argument if first is not `mempty`
-- >>> "ab" <?> "AB"
-- "ab"
(<?>) :: (Monoid a, Eq a) => a -> a -> a
mX <?> mY | (mX, mY) == (mempty, mempty) = mempty
          | (mX, mY) == (mempty, mY)     = mY
          | otherwise                    = mX

options :: Int -> [OptDescr (Flags -> Flags)]
options termSize =
  [ Option ['t']
           ["filetype", "ft"]
           (ReqArg ((fileType .~) . unAbbr) "FILETYPE")
           "File type"
  , Option ['m']
           ["module"]
           (ReqArg ((moduleName .~) . T.pack) "MODULE NAME")
           "Name of the library module"
  , Option ['d'] ["debug"] (NoArg (debug .~ True)) "Print debug info"
  , Option ['l']
           ["line-break-length"]
           (ReqArg (\x -> breakLen .~ fromMaybe termSize (readMaybe x)) "LENGTH")
    $ printf "Length of line breaks (default: %i)" termSize
  , Option [] ["stdout-only"] (NoArg (stdoutOnly .~ True)) "Send output to STDOUT only"
  , Option
      ['i']
      ["indent"]
      (ReqArg (\x -> indentType .~ fromMaybe M.Spaces (readMaybe x)) "(Spaces|Tabs)")
    $ printf "Indent type (default: %s)" (show $ defaultFlags ^. indentType)
  ]

compileOpts :: [String] -> IO (Flags, [String])
compileOpts argv = do
  options' <-
    options . maybe 60 (floor . (* 0.6) . fromInteger . Term.width) <$> Term.size
  case getOpt RequireOrder options' argv of
    (_, [], _) -> ioError $ userError $ "'No input file'\n" ++ usageInfo header options'
    (flags, args, []) -> return (foldl (flip id) defaultFlags flags, args)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options'))
  where header = "Usage: vimHelpGen [OPTIONS...] files..."

choose :: [a] -> IO a
choose = undefined

main :: IO ()
main = do

  --let luaDirConts = listDirectory "." >>= maybe (return mempty) getDirectoryContents . ((== "lua") `find`)
  --modName <- if length luaDirConts > 1 
              --then choose =<< luaDirConts
              --else head <$> luaDirConts

  void getChar


  flags <- do
    (flags, files') <- compileOpts =<< getArgs
    termSize        <- maybe 0 Term.width <$> Term.size

    return
      $ (files .~ files')
      . (fileType %~ (<?> (unAbbr $ takeExtension $ head files')))
      . (breakLen %~ (flip boolComb (50 `percentOf` termSize) =<< (< 0)))
      -- . (moduleName %~ (<?> undefined))
      $ flags

  let convertInfo = M.ConvertInfo { M._lineBreakLength = flags ^. breakLen
                                  , M._indentType      = flags ^. indentType
                                  , M._indentLevel     = 0
                                  , M._tags            = []
                                  , M._fileName        = ""
                                  , M._moduleName      = ""
                                  }

  forM_ (flags ^. files) $ \file -> do
    let output (helpTxt, tags) =
          mapM_ T.putStrLn [T.pack file, "-----", helpTxt, "tags:", "-----", tags]

    case flags ^. fileType of
      "markdown" ->
        T.readFile file >>= output . M.converter (convertInfo & M.fileName .~ file)
      _ -> ioError (userError $ "Unknown file type: '" ++ flags ^. fileType ++ "'")
