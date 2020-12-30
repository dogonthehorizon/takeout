{-# LANGUAGE DuplicateRecordFields #-}

module Data.Google.Music.Playlist where

import qualified Data.Bifunctor       as Bifunctor
import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (DefaultOrdered, FromNamedRecord (..),
                                       decodeByName, (.:))
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as IntMap
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector
import           GHC.Generics         (Generic)
import qualified System.Directory     as Dir

type Playlist = IntMap Track

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

vecToPlaylist :: Vector GooglePlaylistEntry -> Playlist
vecToPlaylist v =
  IntMap.fromList . Vector.toList $ fmap (\e -> (playlistIndex e, toTrack e)) v

readPlaylistEntry :: Text -> IO (Either Text (Vector GooglePlaylistEntry))
readPlaylistEntry f = do
  csvData <- decodeByName <$> BL.readFile (T.unpack f)
  return $ snd <$> Bifunctor.first T.pack csvData

readPlaylist :: Text -> IO (Either Text Playlist)
readPlaylist f = do
  files   <- (fmap . fmap) T.pack $ Dir.listDirectory (T.unpack f)
  entries <- sequence $ readPlaylistEntry . (\x -> T.concat [f, x]) <$> files
  return $ (vecToPlaylist . Vector.concat) <$> sequence entries
