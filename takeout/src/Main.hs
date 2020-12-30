module Main where

import           Control.Monad              (join)
import           Control.Monad.Free         (Free (..), liftF)
import           Data.Google.Music.Playlist (Playlist)
import qualified Data.Google.Music.Playlist as Google
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified System.Directory           as Dir
import           Text.Pretty.Simple         (pPrint)

data GMusicF next =
  GetPlaylistNames Text ([Text] -> next)
    | GetPlaylist Text (Playlist -> next)
    deriving (Functor)

type GMusic = Free GMusicF

getPlaylistNames :: Text -> GMusic [Text]
getPlaylistNames t = liftF $ GetPlaylistNames t id

getPlaylist :: Text -> GMusic Playlist
getPlaylist t = liftF $ GetPlaylist t id

interpreterIO :: GMusic a -> IO (Either Text a)
interpreterIO (Pure a                        ) = return . Right $ a
interpreterIO (Free (GetPlaylistNames t next)) = do
  playlistDirs <- fmap T.pack <$> Dir.listDirectory (T.unpack t)
  interpreterIO . next $ playlistDirs
interpreterIO (Free (GetPlaylist t next)) = do
  playlist <- Google.readPlaylist t
  fmap join . sequence $ interpreterIO . next <$> playlist

main :: IO ()
main = do
  result <- interpreterIO $ do
    let
      path =
        "/home/ffreire/Downloads/gplay/Takeout/Google Play Music/Playlists/"
    ps <-
      filter (\x -> not ("Thumbs Up" `T.isInfixOf` x))
      .   fmap (path <>)
      <$> getPlaylistNames path
    sequence $ getPlaylist <$> ps
  pPrint result
