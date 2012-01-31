module Yesod.CoreBot.Bliki.Resources.Wiki where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base
import Yesod.CoreBot.Bliki.Store 
import Yesod.CoreBot.Bliki.Resources.Base
import Yesod.CoreBot.Bliki.Widget.Head

import Data.FileStore
import qualified Data.Text as Text
import Data.Time.Clock.POSIX

data Wiki = Wiki
    { source_data :: Data
    }

mkWiki :: Data -> IO Wiki
mkWiki src_data = return $ Wiki src_data 

-- XXX: needs json representation
getIndexR :: Yesod master => [ Text ] -> GHandler Wiki master RepHtml
getIndexR node_path = do
    liftIO $ putStrLn $ "index for node " ++ show node_path
    wiki <- getYesodSub
    let src_data = source_data wiki
    defaultLayout $ do
        common_head
        sidebar_widget $ nav src_data
        let store_path = foldl (</>) "" $ map Text.unpack node_path
        listing <- liftIO $ directory_listing (store src_data) store_path
        let entry_names = [ Text.pack name | FSFile name      <- listing ]
            node_names  = [ Text.pack name | FSDirectory name <- listing ]
        let bliki_URL e = entry_URL (nav bliki) $ node_path ++ [ e ]
            wiki_URL e = wiki_index_URL (nav bliki) $ node_path ++ [ e ]
        [whamlet|
        <div .wiki_index>
            <ul>/#{store_path}
                $forall node_name <- node_names
                    <li .node_name><a href=#{wiki_URL node_name}>#{node_name}
                $forall entry_name <- entry_names
                    <li .entry_name><a href=#{bliki_URL entry_name}>#{entry_name}
|]

mkYesodSub "Wiki" [] [parseRoutes|
/*Texts  IndexR GET
|]

