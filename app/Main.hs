{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import GHC.Generics

-- import qualified Lib as Lib
import qualified Data.Aeson as Aeson
import qualified System.Directory as Dir
import qualified Options.Generic as Opt
import qualified Data.Time as Time
import qualified Data.Time.Format as Format
import qualified System.FilePath.Posix as FilePath
import qualified Control.Exception.Safe as Ex
import qualified Data.List as List
import qualified Text.Parsec as Parsec
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Lazy as BSLazy

data Args = Args { argsDir :: FilePath Opt.<?> "Directory scanned for log files"
                 } deriving (Generic, Show)

instance Opt.ParseRecord Args

data LogEntry = LogEntry
  { logEntryMood :: !Int
  , logEntryItems :: ![Text.Text]
  } deriving (Show)

data LogFile body = LogFile
  { logFilePath :: !FilePath
  , logFileTs :: !TimeType
  , logFileBody :: body
  } deriving (Show, Eq)

data JsonLogEntry = JsonLogEntry
  { fromNumber :: String
  , body :: String
  } deriving (Generic, Show)

instance Aeson.FromJSON JsonLogEntry
instance Aeson.ToJSON JsonLogEntry

data JsonParseError = JsonParseError Text.Text deriving Show
instance Ex.Exception JsonParseError

data BodyParseError = BodyParseError Parsec.ParseError deriving Show
instance Ex.Exception BodyParseError

type TimeType = Time.UTCTime

main :: IO ()
main = do
  (args :: Args) <- Opt.getRecord "Moodbot Log File Parser"
  dirListing <- listDirectoryAbsolute (Opt.unHelpful (argsDir args))
  logFiles <- traverse parseDirListing dirListing
  let sortedLogFiles = List.sortOn (logFileTs) logFiles
  print sortedLogFiles
  readLogFiles <- traverse readLogEntry sortedLogFiles
  logEntries <- traverse parseLogEntryBody readLogFiles
  _ <- traverse print logEntries
  return ()

logEntryToCSVString :: LogEntry -> Text.Text
logEntryToCSVString logEntry =
  (Text.pack . show $ logEntryMood logEntry) `Text.append` (Text.concat $ logEntryItems logEntry)

parseDirListing :: (Ex.MonadThrow m) => FilePath -> m (LogFile ())
parseDirListing fp = do
  parsedTime <- extractTime fp
  return $ LogFile {logFilePath = fp, logFileTs = parsedTime, logFileBody = ()}

readLogEntry :: LogFile () -> IO (LogFile JsonLogEntry)
readLogEntry logFile = do
  jsonBody <- Text.readFile (logFilePath logFile) >>= extractEntryFileBody
  return $ logFile {logFileBody = jsonBody}

parseLogEntryBody :: (Ex.MonadThrow m) => LogFile JsonLogEntry -> m (LogFile LogEntry)
parseLogEntryBody logFile =
  let entryBody = Text.pack . body . logFileBody $ logFile in
    case Parsec.runParser entryParser () "" entryBody of
      Right parsed -> return $ logFile {logFileBody = parsed}
      Left l -> Ex.throw $ BodyParseError l

-- | Extract and parse the timestamp in the filename.
-- /foo/bar/123.txt will be interpreted as 123 seconds since the Unix epoch.
-- Throws if the timestamp cannot be parsed.
extractTime :: (Format.ParseTime p, Ex.MonadThrow m) => FilePath -> m p
extractTime path =
  Format.parseTimeM True Format.defaultTimeLocale "%s" (getEpoch path)
  where getEpoch = FilePath.dropExtension . FilePath.takeFileName

-- | Like `sortOn`, but the extraction function can have an effect.
sortOnM :: (Ord o, Monad m) => (a -> m o) -> [a] -> m [a]
sortOnM f xs = do
  xsOrd <- traverse f xs
  let paired = zip xs xsOrd
  let sortedPaired = List.sortOn snd paired
  return $ fmap fst sortedPaired

listDirectoryAbsolute :: FilePath -> IO [FilePath]
listDirectoryAbsolute fp = do
  paths <- Dir.listDirectory fp
  return $ fmap (\path -> fp ++ "/" ++ path) paths

entryParser :: Parsec.Parsec Text.Text () LogEntry
entryParser = LogEntry
  <$> (read <$> Parsec.many1 Parsec.digit)
  <*> (Parsec.spaces *> (Text.pack <$> Parsec.many1 (Parsec.noneOf ",")) `Parsec.sepBy` (Parsec.char ','))
  <* Parsec.eof

extractEntryFileBody :: Ex.MonadThrow m => Text.Text -> m JsonLogEntry
extractEntryFileBody txt =
  case Aeson.decode . BSLazy.fromStrict . Text.encodeUtf8 $ txt of
    Just entry -> return entry
    Nothing -> Ex.throw (JsonParseError txt)
