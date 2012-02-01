module Yesod.CoreBot.Bliki.Config where

import Yesod.CoreBot.Bliki.Prelude

import Control.Monad.Reader.Class

import Data.FileStore ( RevisionId )

import qualified Data.Text as Text

data Static 
    = UseServer String
    | UseDir FilePath

data AnyRoute where
    AnyRoute :: forall r . RenderRoute r => Route r -> AnyRoute

data DataRoutes master where
    DataRoutes :: { latest_R :: Route master
                  , update_log_R :: Route master
                  , entry_latest_R :: Route master
                  , blog_R :: Route master
                  } -> DataRoutes master

data BlogRoutes master where
    BlogRoutes :: { blog_index_R :: Route master } -> BlogRoutes master

data WikiRoutes master where
    WikiRoutes :: { wiki_index_R :: Route master } -> WikiRoutes master

data StaticRoutes master where
    StaticRoutes :: { file_R :: Route master } -> StaticRoutes master

-- I don't think there is a way to do subsites of subsites in Yesod? 
-- Widgets that need to build links need the master routes?
data Config master where
    Config :: { store_dir :: FilePath
              , cache_dir :: FilePath
              , data_routes   :: DataRoutes master
              , blog_routes   :: BlogRoutes master
              , wiki_routes   :: WikiRoutes master
              , static_routes :: StaticRoutes master
              , static_config :: Static
              } -> Config master

class ( Applicative m, MonadReader m, Yesod master, EnvType m ~ Config master ) => ConfigM master m
instance ( Applicative m, MonadReader m, Yesod master, EnvType m ~ Config master ) => ConfigM master m

node_markdown_path :: Yesod master => Config master -> FilePath -> FilePath
node_markdown_path config node_path =
    store_dir config </> node_path

blog_HTML_path :: Yesod master => Config master -> RevisionId -> FilePath
blog_HTML_path config rev_ID = 
    cache_dir config </> rev_ID </> "_log"

node_HTML_path :: Yesod master => Config master-> RevisionId -> FilePath -> FilePath
node_HTML_path config rev_ID node_path = 
    cache_dir config </> rev_ID </> node_path

revision_blog_URL :: ConfigM master m => RevisionId -> m String
revision_blog_URL = return undefined
    
entry_at_rev_URL :: ConfigM master m => String -> RevisionId -> m String
entry_at_rev_URL entry_path rev_ID = return undefined

