module Yesod.CoreBot.Bliki.Config where

import Yesod.CoreBot.Bliki.Prelude

import Control.Monad.Reader.Class

import Data.FileStore ( RevisionId )

import System.Directory ( createDirectory
                        , doesDirectoryExist
                        , removeDirectoryRecursive 
                        )

type LayoutHandler = forall sub a . GWidget sub a () -> GHandler sub a RepHtml

data Config = Config
    { store_dir :: FilePath
    , cache_dir :: FilePath
    , static_base_URL :: String
    , layout :: LayoutHandler
    , mk_node_data_URL :: [ Text ] -> [ Text ]
    , mk_wiki_index_URL :: [ Text ] -> [ Text ]
    , 
    }

mk_config :: FilePath -> FilePath -> String -> LayoutHandler -> IO Config
mk_config store_dir cache_dir static_base_URL layout = do
    -- clear memoization store
    should_clear_memo_store <- doesDirectoryExist cache_dir
    when should_clear_memo_store $ removeDirectoryRecursive cache_dir
    createDirectory cache_dir
    return $ Config store_dir 
                    cache_dir 
                    static_base_URL 
                    layout 
                    undefined 
                    undefined

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

