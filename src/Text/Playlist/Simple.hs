--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | A simple playlist:
--
-- > Title = URL
--
-- With optional comments that begin with a @#@ sign and continue to
-- the end of the line.  A comment can be on its own line or after the
-- URL portion of a stream definition.  I probably should fix this
-- because it means that URLs cannot contain @#@ characters.
module Text.Playlist.Simple (simplePlaylist) where

--------------------------------------------------------------------------------
import Control.Applicative hiding ((<|>))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Text.Playlist.Internal
import Control.Monad (void)

--------------------------------------------------------------------------------
-- | Skip comments.  Comments begin with a pound/number sign and run
-- to the end of the line or the end of the file if the file doesn't
-- end in a newline.
comment :: Parser ()
comment = char '#' >> void (manyTill anyChar end) <?> "comment"
  where end = void newline <|> eof

--------------------------------------------------------------------------------
-- | Parse the title of a stream.
streamTitle :: Parser Title
streamTitle = T.strip . T.pack <$> many1 (noneOf "#=\n") <?> "stream title"

--------------------------------------------------------------------------------
-- | Parse the URL for a stream.
streamURL :: Parser URL
streamURL = T.strip . T.pack <$> many1 (noneOf "#\n") <?> "stream URL"

--------------------------------------------------------------------------------
-- | Extract a 'Stream' from the current line.
stream :: Parser Stream
stream = Stream <$> (streamTitle <* char '=') <*> (streamURL <* spaces)

--------------------------------------------------------------------------------
-- | Parse a line from the playlist file.  A line can either be a
-- stream definition or a comment.  This parser will skip over all
-- whitespace and comments and either produce a 'Stream' or fail.
line :: Parser (Maybe Stream)
line = spaces >> ((comment >> empty) <|> Just <$> stream) <?> "stream or comment"

--------------------------------------------------------------------------------
-- | A parser that can process a simple playlist.
simplePlaylist :: Parser Playlist
simplePlaylist = catMaybes <$> many1 line <* eof <?> "simple playlist"
