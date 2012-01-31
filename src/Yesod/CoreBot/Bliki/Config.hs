module Yesod.CoreBot.Bliki.Config where

import Yesod.CoreBot.Bliki.Prelude

import Control.Monad.Reader.Class

import Data.FileStore ( RevisionId )

data Config = Config
    { store_dir :: FilePath
    , cache_dir :: FilePath
    }

class ( Applicative m, MonadReader m, EnvType m ~ Config ) => ConfigM m
instance ( Applicative m, MonadReader m, EnvType m ~ Config ) => ConfigM m

node_markdown_path :: Config -> FilePath -> FilePath
node_markdown_path config node_path =
    store_dir config </> node_path

blog_HTML_path :: Config -> RevisionId -> FilePath
blog_HTML_path config rev_ID = 
    cache_dir config </> rev_ID </> "_log"

node_HTML_path :: Config -> RevisionId -> FilePath -> FilePath
node_HTML_path config rev_ID node_path = 
    cache_dir config </> rev_ID </> node_path

revision_blog_URL :: ConfigM m => RevisionId -> m String
revision_blog_URL = return undefined
    
entry_at_rev_URL :: ConfigM m => String -> RevisionId -> m String
entry_at_rev_URL entry_path rev_ID = return undefined

