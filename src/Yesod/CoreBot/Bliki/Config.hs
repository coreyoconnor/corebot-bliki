module Yesod.CoreBot.Bliki.Config where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.DB
import Yesod.CoreBot.Bliki.Store

import Control.Monad.Reader.Class

import Data.FileStore ( RevisionId )

import qualified Data.Text as Text

data Data_ master = Data 
    { config             :: Config master
    , store              :: Store
    , update_thread_ID   :: ThreadId
    , db_ref             :: IORef DB
    }

data Blog_ master = Blog ( Data_ master )

data Wiki_ master = Wiki ( Data_ master )

data Static 
    = UseServer String
    | UseDir FilePath

data AnyRoute where
    AnyRoute :: forall r . RenderRoute r => Route r -> AnyRoute

-- I don't think there is a way to do subsites of subsites in Yesod? 
-- Widgets that need to build links need the master routes?
data Config master where
    Config :: ( Yesod master, RenderRoute (Route master) ) => 
        { store_dir :: FilePath
        , cache_dir :: FilePath
        , data_routes   :: Route ( Data_ master ) -> Route master
        , blog_routes   :: Route ( Blog_ master ) -> Route master
        , wiki_routes   :: Route ( Wiki_ master ) -> Route master
        , static_routes :: Route Static -> Route master
        , static_config :: Static
        , site :: master
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

