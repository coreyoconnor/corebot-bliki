module Yesod.CoreBot.Bliki.DB where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config

import Control.Monad.Reader
import Control.Monad.State.Strict 

import Data.FileStore ( Revision(..) 
                      , TimeRange(..)
                      , Resource
                      , RevisionId
                      )
import qualified Data.FileStore as FileStore

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Time.Clock

data DataUpdate 
    = Tweet
        { update_rev_ID :: RevisionId 
        , tweet_text    :: String
        }
    | BlogAdded
        { update_rev_ID :: RevisionId 
        , blog_text     :: String
        }
    | EntryAdded
        { update_rev_ID     :: RevisionId
        , update_entry_path :: FilePath
        }
    | EntryChanged 
        { update_rev_ID     :: RevisionId 
        , update_entry_path :: FilePath
        }
    | Wibble
        { update_rev_ID :: RevisionId
        }
    deriving ( Show, Eq )

data Bloggable = WikiBloggable
    { blog_entry  :: String
    , view_rev    :: RevisionId
    , prev_bloggable   :: Maybe Bloggable
    } | UpdateBloggable
    { blog_str    :: String
    , source_rev  :: RevisionId
    , prev_bloggable :: Maybe Bloggable
    }

-- XXX The order is always assumed to be from newest to oldest in the lists. This should be
-- checked/enforced.
-- XXX Use a database store like SQLite
data DB = DB
    { raw_history       :: [ Revision ]
    , update_log        :: [ DataUpdate ]
    , latest_revisions  :: Map FilePath RevisionId
    , bloggables        :: [ Bloggable ]
    }

history_to_updates :: [ Revision ] -> [ DataUpdate ]
history_to_updates (r : rs) = revision_to_updates r ++ history_to_updates rs

revision_to_updates :: Revision -> [ DataUpdate ]
revision_to_updates r = 
    let txt = FileStore.revDescription r
        rev_ID = FileStore.revId r
    in map ( classify_change rev_ID ) ( FileStore.revChanges r )
       ++ case length txt of
            txt_len | txt_len > 200 -> [ BlogAdded rev_ID txt ]
                    | txt_len > 8   -> [ Tweet     rev_ID txt ]
                    | otherwise     -> [ Wibble    rev_ID     ]

classify_change rev_ID  (FileStore.Added   page_path) = 
    case head page_path of 
        '.' -> Wibble rev_ID
        _   -> EntryAdded rev_ID page_path
classify_change rev_ID (FileStore.Deleted  page_path) = Wibble rev_ID
classify_change rev_ID (FileStore.Modified page_path) = 
    case head page_path of
        '.' -> Wibble rev_ID
        _   -> EntryChanged rev_ID page_path

head_time :: IORef DB -> IO UTCTime
head_time db_ref = do
    db <- readIORef db_ref
    return $ FileStore.revDateTime $ head $ raw_history db

