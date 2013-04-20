--------------------------------------------------------------------------------
module Text.Playlist.Internal
       ( Stream (..)
       , Playlist
       , Title
       , URL
       ) where

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
type Title = Text -- ^ Stream title/name.
type URL   = Text -- ^ URL for the stream.

--------------------------------------------------------------------------------
-- | An Internet radio station or music stream is basically just a
-- title and URL.
data Stream = Stream Title URL deriving (Show)

--------------------------------------------------------------------------------
-- | A playlist is a list of streams to play or choose from.
type Playlist = [Stream]
