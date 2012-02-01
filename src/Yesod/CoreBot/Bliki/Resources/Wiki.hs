module Yesod.CoreBot.Bliki.Resources.Wiki where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base
import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Store 
import Yesod.CoreBot.Bliki.Resources.Base
import Yesod.CoreBot.Bliki.Widget.Head

import Control.Monad.State.Strict

import Data.FileStore
import qualified Data.Text as Text
import Data.Time.Clock.POSIX

mk_wiki :: Data_ master -> IO ( Wiki_ master )
mk_wiki src_data = return $ Wiki src_data 

-- XXX: needs json representation
getWikiIndexR :: Yesod master => [ Text ] -> GHandler ( Wiki_ master ) master RepHtml
getWikiIndexR node_path = do
    liftIO $ putStrLn $ "index for node " ++ show node_path
    wiki@(Wiki src_data) <- getYesodSub
    defaultLayout $ do
        let store_path = foldl (</>) "" $ map Text.unpack node_path
        listing <- evalStateT (directory_listing store_path) ( store src_data )
        let entry_names = [ Text.pack name | FSFile name      <- listing ]
            node_names  = [ Text.pack name | FSDirectory name <- listing ]
        let data_URL = latest_route ( data_routes (config src_data) )
            index_URL = wiki_index_route ( wiki_routes (config src_data) )
        [whamlet|
        <div .wiki_index>
            <ul>/#{store_path}
                $forall node_name <- node_names
                    <li .node_name><a href=@{index_URL}/#{store_path}/#{node_name}>#{node_name}
                $forall entry_name <- entry_names
                    <li .entry_name><a href=@{data_URL}/#{store_path}/#{entry_name}>#{entry_name}
|]

mkYesodSubDispatch "Wiki_ master" [] [parseRoutes|
/*Texts  WikiIndexR GET
|]

