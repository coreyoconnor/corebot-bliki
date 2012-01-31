module Yesod.CoreBot.Bliki.Store where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config

import Control.Monad.State.Class

import qualified Data.ByteString.Lazy as B

import qualified Data.FileStore as FileStore

import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import System.Directory

-- XXX: The inner FileStore is not actually pure.
data Store = Store
    { filestore  :: FileStore
    }

class ( MonadIO m, Applicative m, MonadState m, StateType m ~ Store ) => StoreM m
instance ( MonadIO m, Applicative m, MonadState m, StateType m ~ Store ) => StoreM m

directory_listing :: StoreM m => FilePath -> m [ Resource ]
directory_listing node_path = do
    store <- get
    liftIO $ FileStore.directory (filestore store) node_path

cache_str :: FilePath -> String -> IO ()
cache_str out_path str = do
    let text = TL.pack str
        bs = TL.encodeUtf8 text
        tmp_out   = out_path ++ ".tmp"
    createDirectoryIfMissing True (takeDirectory tmp_out)
    B.writeFile tmp_out bs
    renameFile tmp_out out_path
    return ()

data_for_node_rev :: StoreM m
                  => FilePath 
                  -> RevisionId 
                  -> m B.ByteString
data_for_node_rev node_path rev_ID = do
    fs <- gets filestore
    liftIO $ FileStore.smartRetrieve fs True node_path (Just rev_ID) 

