module Yesod.CoreBot.Bliki.Config where

import Yesod.CoreBot.Bliki.Prelude

import Control.Monad.Reader.Class

import Data.FileStore ( RevisionId )

import System.Directory ( createDirectory
                        , doesDirectoryExist
                        , removeDirectoryRecursive 
                        )

type LayoutHandler = forall sub a . GWidget sub a () -> GHandler sub a RepHtml

data DataRoutes master = DataRoutes
    { latest_route   :: Route master
    , entry_latest_R :: Route master
    }

data BlogRoutes master = BlogRoutes
    { blog_index_route :: Route master
    }

data WikiRoutes master = WikiRoutes
    { wiki_index_route :: Route master
    }

data StaticRoutes master = StaticRoutes
    { static_file_route :: Route master
    }

data Static 
    = UseServer String
    | UseDir FilePath

-- I don't think there is a way to do subsites of subsites in Yesod? 
-- Widgets that need to build links need the master routes?
data Config master = Config
    { store_dir :: FilePath
    , cache_dir :: FilePath
    , layout :: LayoutHandler
    , data_routes   :: DataRoutes master
    , blog_routes   :: BlogRoutes master
    , wiki_routes   :: WikiRoutes master
    , static_routes :: StaticRoutes master
    , static_config :: Static
    }

mk_config :: Yesod master => FilePath -> FilePath -> LayoutHandler -> IO ( Config master )
mk_config store_dir cache_dir layout = do
    -- clear memoization store
    should_clear_memo_store <- doesDirectoryExist cache_dir
    when should_clear_memo_store $ removeDirectoryRecursive cache_dir
    createDirectory cache_dir
    return $ Config store_dir 
                    cache_dir 
                    layout 
                    undefined 
                    undefined
                    undefined
                    undefined
                    undefined

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

