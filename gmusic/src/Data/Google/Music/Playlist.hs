{-# LANGUAGE DuplicateRecordFields #-}

module Data.Google.Music.Playlist (
  readPlaylist,
  CanBeTrack(..),
  Playlist,
  PlaylistMeta,
  TrackListing,
  Track(..),
  Shareable
  ) where

import qualified Data.Bifunctor       as Bifunctor
import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (DefaultOrdered, FromField (parseField),
                                       FromNamedRecord (..), decodeByName, (.:))
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as IntMap
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector
import           GHC.Generics         (Generic)
import qualified System.Directory     as Dir

data Playlist = Playlist {
    metadata :: !PlaylistMeta,
    tracks :: !TrackListing
  } deriving (Show)

type PlaylistMeta = GooglePlaylistMetadataEntry
type TrackListing = IntMap Track

data Track = Track {
    title :: !Text,
    album :: !Text,
    artist :: !Text
  } deriving (Show)

class CanBeTrack a where
  toTrack :: a -> Track

instance CanBeTrack GooglePlaylistEntry where
  toTrack GooglePlaylistEntry { title = gTitle, album = gAlbum, artist = gArtist  } =
    Track {
        title = gTitle,
        album = gAlbum,
        artist = gArtist
      }

data Shareable = Yes | No deriving (Show)

instance FromField Shareable where
  parseField "Yes" = pure Yes
  parseField _ = pure No

data GooglePlaylistMetadataEntry = GooglePlaylistMetadataEntry {
    title :: !Text,
    description :: !Text,
    shared :: !Shareable
  } deriving (Generic, Show)

instance FromNamedRecord GooglePlaylistMetadataEntry where
  parseNamedRecord r = GooglePlaylistMetadataEntry
    <$> r .: "Title"
    <*> r .: "Description"
    <*> r .: "Shared"

data GooglePlaylistEntry = GooglePlaylistEntry {
    title :: !Text,
    album :: !Text,
    artist :: !Text,
    duration :: !Double,
    rating :: !Int,
    playCount :: !Int,
    playlistIndex :: !Int
  } deriving (Generic, Show)

instance FromNamedRecord GooglePlaylistEntry where
  parseNamedRecord r = GooglePlaylistEntry
    <$> r .: "Title"
    <*> r .: "Album"
    <*> r .: "Artist"
    <*> r .: "Duration (ms)"
    <*> r .: "Rating"
    <*> r .: "Play Count"
    <*> r .: "Playlist Index"

instance DefaultOrdered GooglePlaylistEntry

vecToTrackListing :: Vector GooglePlaylistEntry -> TrackListing
vecToTrackListing v =
  IntMap.fromList . Vector.toList $ fmap (\e -> (playlistIndex e, toTrack e)) v

readCsv :: FromNamedRecord a => Text -> IO (Either Text (Vector a))
readCsv f = do
  csvData <- decodeByName <$> BL.readFile (T.unpack f)
  return $ snd <$> Bifunctor.first T.pack csvData

readPlaylistEntry :: Text -> IO (Either Text (Vector GooglePlaylistEntry))
readPlaylistEntry = readCsv

readMetadataEntry
  :: Text -> IO (Either Text (Vector GooglePlaylistMetadataEntry))
readMetadataEntry = readCsv

readPlaylist :: Text -> IO (Either Text Playlist)
readPlaylist f = do
  metadata <- fmap Vector.head <$> readMetadataEntry (f <> "Metadata.csv")
  files    <- fmap T.pack <$> Dir.listDirectory (T.unpack $ f <> "Tracks/")
  entries  <-
    sequence $ readPlaylistEntry . (\x -> f <> "Tracks/" <> x) <$> files
  return
    $   Playlist
    <$> metadata
    <*> (vecToTrackListing . Vector.concat <$> sequence entries)
