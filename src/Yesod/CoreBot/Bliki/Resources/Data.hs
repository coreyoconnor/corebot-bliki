{-# LANGUAGE NamedFieldPuns #-}
module Yesod.CoreBot.Bliki.Resources.Data where

import Yesod.CoreBot.Bliki.Prelude 

import Yesod.CoreBot.Bliki.Resources.Base 

import Yesod.CoreBot.Bliki.Cache.UpdateHTML
import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.DB
import Yesod.CoreBot.Bliki.Store

import Control.Monad.Reader
import Control.Monad.State.Strict 

import qualified Data.FileStore as FileStore

import Data.Map ( Map )
import qualified Data.Map as Map

import qualified Data.Text               as T

import Data.Time.Clock
import Data.Time.Clock.POSIX

-- XXX: should be a RWST
type DataM a = StateT DB ( StateT Store (ReaderT Config IO) ) a

process_revisions :: ( Config, Store ) -> IORef DB -> [ Revision ] -> IO ()
process_revisions ( config, store ) db_ref rs = do
    db  <- readIORef db_ref
    let db_mod = execStateT (apply_revisions rs) db
    db' <- runReaderT ( evalStateT db_mod store ) config 
    writeIORef db_ref db'

apply_revisions :: [ Revision ] -> DataM ()
apply_revisions [] = do
    return ()
apply_revisions [ r ] = do
    let new_updates = revision_to_updates r
    apply_updates new_updates
    modify $ \db -> db { raw_history = r : raw_history db }
    return ()
apply_revisions (r : r_prev : rs) = do
    apply_revisions ( r_prev : rs )
    let new_updates = revision_to_updates r
    apply_updates new_updates
    modify $ \db -> db { raw_history = r : raw_history db }
    return ()

apply_updates :: [ DataUpdate ] -> DataM ()
apply_updates [] = do
    return ()
apply_updates ( u : us ) = do
    apply_updates us
    modify $ \db -> db { update_log = u : update_log db }
    f u 
    where 
        f ( Wibble _ ) = do
            return ()

        f ( Tweet _ _ ) = do
            return ()

        f ( BlogAdded update_rev_ID blog_str ) = do
            b <- add_bloggable $ UpdateBloggable blog_str update_rev_ID
            build_blog_HTML ( prev_bloggable b ) update_rev_ID blog_str

        f ( EntryAdded update_rev_ID update_entry_path ) = do
            revs <- gets latest_revisions
            let revs' = Map.insert update_entry_path update_rev_ID revs
            modify $ \db -> db { latest_revisions = revs' }
            b <- add_bloggable $ WikiBloggable update_entry_path update_rev_ID
            lift $ build_node_HTML (prev_bloggable b) update_rev_ID update_entry_path

        f ( EntryChanged update_rev_ID update_entry_path ) = do 
            revs <- gets latest_revisions
            let revs' = Map.insert update_entry_path update_rev_ID revs
            modify $ \db -> db { latest_revisions = revs' }
            bs <- gets bloggables
            b <- case bs of
                ( WikiBloggable blog_entry view_rev prev_bloggable : rest_bs )
                    | blog_entry == update_entry_path -> do
                        let b' = WikiBloggable blog_entry update_rev_ID prev_bloggable
                        modify $ \db -> db { bloggables = b' : rest_bs }
                        return b'
                _ -> do
                    add_bloggable $ WikiBloggable update_entry_path update_rev_ID 
            lift $ build_node_HTML ( prev_bloggable b ) update_rev_ID update_entry_path

add_bloggable :: ( Maybe Bloggable -> Bloggable ) -> DataM Bloggable
add_bloggable fb = do
    bs <- gets bloggables
    let b = case bs of
                []         -> fb Nothing
                b_prev : _ -> fb $ Just b_prev
    modify $ \db -> db { bloggables = b : bs }
    return b

-- XXX: Should be event driven but that'd be harder
update_thread :: ( Config, Store ) -> IORef DB  -> IO ()
update_thread ( config, store ) db_ref = do
    -- XXX: Lower bound to FileStore.history is not exclusive
    let inc_a_bit = addUTCTime (fromInteger 1)
    prev_time_ref <- newIORef =<< return . inc_a_bit =<< head_time db_ref
    let update_thread_ = do
            prev_time <- readIORef prev_time_ref
            putStrLn $ "probing for changes since " ++ show prev_time
            rs <- FileStore.history ( filestore store ) 
                                    [] 
                                    ( TimeRange (Just prev_time) 
                                                Nothing
                                    )
            case null rs of
                True -> return ()
                False -> do
                    putStrLn "found updates"
                    process_revisions ( config, store ) db_ref rs
                    writeIORef prev_time_ref =<< return . inc_a_bit 
                                             =<< head_time db_ref
            -- delay before probing for updates again
            threadDelay 10000000
    forever update_thread_

mk_data :: Config -> IO Data
mk_data config = do
    let filestore = FileStore.gitFileStore $ store_dir config
        store = Store { filestore = filestore }
        empty_db = DB [] [] Map.empty []
    initial_history <- FileStore.history filestore [] (TimeRange Nothing Nothing) 
    -- collect initial data
    let db_0_build = execStateT (apply_revisions initial_history) empty_db
    db_ref <- newIORef =<< runReaderT (evalStateT db_0_build store) config
    -- XXX: Only because store is not pure value but a reference
    the_ID <- forkIO $ update_thread ( config, store ) db_ref
    return Data { config            = config
                , store             = store
                , update_thread_ID  = the_ID
                , db_ref            = db_ref
                }

node_HTML_content :: Data -> DB -> FilePath -> Content
node_HTML_content src_data db node_path = 
    let Just rev_ID = Map.lookup node_path ( latest_revisions db )
        out_path = node_HTML_path (config src_data) rev_ID node_path
    in ContentFile out_path Nothing

blog_HTML_content :: Data -> RevisionId -> Content
blog_HTML_content src_data rev_ID = 
    let out_path = blog_HTML_path (config src_data) rev_ID
    in ContentFile out_path Nothing

getBlogR  :: Yesod master => RevisionId -> GHandler Data master [(ContentType, Content)]
getBlogR rev_ID = do
    src_data <- getYesodSub
    let out_HTML_content = blog_HTML_content src_data rev_ID
    return [ ( typeHtml, out_HTML_content )
           ]
    
getLatestR :: Yesod master => GHandler Data master [(ContentType, Content)]
getLatestR = do
    src_data <- getYesodSub 
    db <- liftIO $ readIORef $ db_ref src_data
    let latest = head $ bloggables db
    case latest of
        UpdateBloggable blog_str source_rev _ -> do
            let out_HTML = blog_HTML_content src_data source_rev
            return [ ( typeHtml, out_HTML )
                   , ( typePlain, toContent blog_str )
                   ]
        WikiBloggable blog_entry _ _  -> do
            let markdown_path = node_markdown_path (config src_data) blog_entry
            let out_HTML  = node_HTML_content src_data db blog_entry
            return [ ( typeHtml , out_HTML )
                   , ( typePlain, ContentFile markdown_path Nothing ) 
                   ]

getUpdateLogR :: Yesod master => GHandler Data master RepJson
getUpdateLogR = do
    jsonToRepJson $ toJSON ()

getEntryRevR :: Yesod master 
             => RevisionId 
             -> [ Text ] 
             -> GHandler Data master [(ContentType, Content)]
getEntryRevR rev_ID entry_path_texts = do
    src_data <- getYesodSub
    let ( first_path : rest_paths ) = map T.unpack entry_path_texts
        node_path = foldl (</>) first_path rest_paths
    let p = node_HTML_path (config src_data) rev_ID node_path
    let markdown_path = node_markdown_path (config src_data) node_path
    return [ ( typeHtml , ContentFile p Nothing  )
           , ( typePlain, ContentFile markdown_path Nothing ) 
           ]
    
getEntryLatestR :: Yesod master => [ Text ] -> GHandler Data master [(ContentType, Content)]
getEntryLatestR entry_path_texts = do
    let ( first_path : rest_paths ) = map T.unpack entry_path_texts
        node_path = foldl (</>) first_path rest_paths
    src_data <- getYesodSub
    db <- liftIO $ readIORef $ db_ref src_data
    let x = Map.lookup node_path ( latest_revisions db )
    case x of
        Nothing -> do
            liftIO $ putStrLn $ "no node at path " ++ show node_path
            fail $ "no node at path " ++ show node_path
        Just rev_ID -> do
            liftIO $ putStrLn $ "latest rev of " ++ show node_path ++ " is " ++ show rev_ID
            getEntryRevR rev_ID entry_path_texts

mkYesodSubDispatch "Data" [] [parseRoutes|
/latest                     LatestR      GET
/                           UpdateLogR   GET
/entry/*Texts               EntryLatestR GET
/blog/#RevisionId           BlogR        GET 
/rev/#RevisionId/*Texts     EntryRevR    GET
|]

