{-# LANGUAGE NamedFieldPuns #-}
module Yesod.CoreBot.Bliki.Resources.Data where

import Yesod.CoreBot.Bliki.Prelude 

import Yesod.CoreBot.Bliki.Resources.Base 

import Yesod.CoreBot.Bliki.DB
import Yesod.CoreBot.Bliki.Store
import Yesod.CoreBot.Bliki.Cache.UpdateHTML

import Control.Monad.Reader
import Control.Monad.State.Strict 

import Data.FileStore ( FileStore
                      , Revision(..) 
                      , TimeRange(..)
                      , Resource
                      , RevisionId
                      )
import qualified Data.FileStore as FileStore

import Data.Map ( Map )
import qualified Data.Map as Map

import qualified Data.Text               as T

import Data.Time.Clock
import Data.Time.Clock.POSIX

import System.Directory ( createDirectory
                        , doesDirectoryExist
                        , removeDirectoryRecursive 
                        )

process_revisions :: ( Nav, Store ) -> IORef DB -> [ Revision ] -> IO ()
process_revisions ( nav, store ) db_ref rs = do
    db  <- readIORef db_ref
    db' <- runReaderT ( execStateT (apply_revisions rs) 
                                   db
                      ) ( nav, store )
    writeIORef db_ref db'

apply_revisions :: [ Revision ] -> BlikiM ()
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

apply_updates :: [ DataUpdate ] -> BlikiM ()
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
            build_node_HTML (prev_bloggable b) update_rev_ID update_entry_path

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
            build_node_HTML ( prev_bloggable b ) update_rev_ID update_entry_path

add_bloggable :: ( Maybe Bloggable -> Bloggable ) -> BlikiM Bloggable
add_bloggable fb = do
    bs <- gets bloggables
    let b = case bs of
                []         -> fb Nothing
                b_prev : _ -> fb $ Just b_prev
    modify $ \db -> db { bloggables = b : bs }
    return b

-- XXX: Should be event driven but that'd be harder
update_thread :: ( Nav, Store ) -> IORef DB  -> IO ()
update_thread ( nav, store ) db_ref = do
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
                    process_revisions ( nav, store ) db_ref rs
                    writeIORef prev_time_ref =<< return . inc_a_bit 
                                             =<< head_time db_ref
            -- delay before probing for updates again
            threadDelay 10000000
    forever update_thread_

mk_data :: FilePath-> FilePath -> Config -> IO Data
mk_data in_store_path in_cache_path config = do
    -- clear memoization store
    should_clear_memo_stor <- doesDirectoryExist in_cache_path
    when should_clear_memo_stor $ removeDirectoryRecursive in_cache_path
    createDirectory in_cache_path
    --
    let filestore = FileStore.gitFileStore in_store_path
        store = Store { store_path = in_store_path
                      , cache_path = in_cache_path
                      , filestore  = filestore
                      }
        db_0 = DB [] [] Map.empty []
    history <-  FileStore.history filestore [] (TimeRange Nothing Nothing) 
    -- collect initial data
    db_ref <- newIORef =<< runReaderT ( execStateT (apply_revisions history) 
                                                   db_0 
                                      ) ( nav, store )
    the_ID <- forkIO (update_thread ( nav, store ) db_ref) 
    return Bliki { store            = store
                 , update_thread_ID = the_ID
                 , db_ref           = db_ref
                 , nav              = nav
                 }

node_html :: Bliki -> DB -> FilePath -> Content
node_html bliki db node_path = 
    let Just rev_ID = Map.lookup node_path ( latest_revisions db )
        out_path = html_data_path (store bliki) rev_ID node_path
    in ContentFile out_path Nothing

blog_html :: Bliki -> RevisionId -> Content
blog_html bliki rev_ID = 
    let out_path = blog_html_path (store bliki) rev_ID
    in ContentFile out_path Nothing

getBlogR  :: Yesod master => RevisionId -> GHandler Bliki master [(ContentType, Content)]
getBlogR rev_ID = do
    bliki <- getYesodSub
    let html_content = blog_html bliki rev_ID
    return [ ( typeHtml, html_content )
           ]
    
getLatestR :: Yesod master => GHandler Bliki master [(ContentType, Content)]
getLatestR = do
    bliki <- getYesodSub 
    db <- liftIO $ readIORef $ db_ref bliki
    let latest = head $ bloggables db
    case latest of
        UpdateBloggable blog_str source_rev _ -> do
            let html_content = blog_html bliki source_rev
            return [ ( typeHtml, html_content )
                   , ( typePlain, toContent blog_str )
                   ]
        WikiBloggable blog_entry _ _  -> do
            let markdown_path = node_markdown_path (store bliki) blog_entry
            let html_content  = node_html bliki db blog_entry
            return [ ( typeHtml , html_content )
                   , ( typePlain, ContentFile markdown_path Nothing ) 
                   ]

getUpdateLogR :: Yesod master => GHandler Bliki master RepJson
getUpdateLogR = do
    jsonToRepJson $ toJSON ()

getEntryRevR :: Yesod master 
             => RevisionId 
             -> [ Text ] 
             -> GHandler Bliki master [(ContentType, Content)]
getEntryRevR rev_ID entry_path_texts = do
    bliki <- getYesodSub
    let ( first_path : rest_paths ) = map T.unpack entry_path_texts
        node_path = foldl (</>) first_path rest_paths
    let p  = html_data_path (store bliki) rev_ID node_path
    let markdown_path = node_markdown_path (store bliki) node_path
    return [ ( typeHtml , ContentFile p Nothing  )
           , ( typePlain, ContentFile markdown_path Nothing ) 
           ]
    
getEntryLatestR :: Yesod master => [ Text ] -> GHandler Bliki master [(ContentType, Content)]
getEntryLatestR entry_path_texts = do
    let ( first_path : rest_paths ) = map T.unpack entry_path_texts
        node_path = foldl (</>) first_path rest_paths
    bliki <- getYesodSub
    db <- liftIO $ readIORef $ db_ref bliki
    let x = Map.lookup node_path ( latest_revisions db )
    case x of
        Nothing -> do
            liftIO $ putStrLn $ "no node at path " ++ show node_path
            fail $ "no node at path " ++ show node_path
        Just rev_ID -> do
            liftIO $ putStrLn $ "latest rev of " ++ show node_path ++ " is " ++ show rev_ID
            getEntryRevR rev_ID entry_path_texts

mkYesodSub "Bliki" [] [parseRoutes|
/latest                     LatestR      GET
/                           UpdateLogR   GET
/entry/*Texts               EntryLatestR GET
/blog/#RevisionId           BlogR        GET 
/rev/#RevisionId/*Texts     EntryRevR    GET
|]

