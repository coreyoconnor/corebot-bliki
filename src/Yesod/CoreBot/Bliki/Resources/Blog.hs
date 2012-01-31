module Yesod.CoreBot.Bliki.Resources.Blog where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Resources.Base

import Yesod.CoreBot.Bliki
import Yesod.CoreBot.Bliki.DB 
import Yesod.CoreBot.Bliki.Store 

import Yesod.CoreBot.Bliki.Widget.Head

import qualified Data.Text as Text

data Blog = Blog
    { source_bliki :: Bliki
    }

mkBlog :: Bliki -> IO Blog
mkBlog source_bliki = return $ Blog source_bliki 

-- XXX: needs json representation
getIndexR :: Yesod master => GHandler Blog master RepHtml
getIndexR = do
    blog <- getYesodSub
    let bliki = source_bliki blog
    db <- liftIO $ readIORef $ db_ref bliki
    let updates = take 50 $ bloggables db
        update_summaries = build_summaries updates
        build_summaries [] = []
        build_summaries ( Tweet     _ txt : us ) 
            = [whamlet|
                <p .tweet> #{txt}
            |] : build_summaries us
        build_summaries ( BlogAdded _ txt : us )
            = [whamlet|
                <p .blog_summary> #{take 200 txt}
            |] : build_summaries us
        build_summaries ( EntryAdded _ node_path : us ) = 
            let base_URL = entry_URL (nav bliki) [ ]
            in [whamlet|
                <p .node_update>
                    Added 
                    <a href=#{base_URL}/#{node_path}>#{node_path}
            |] : build_summaries us
        build_summaries ( EntryChanged _ node_path : us ) = 
            let base_URL = entry_URL (nav bliki) [ ]
            in [whamlet|
                <p .node_update>
                    Changed 
                    <a href=#{base_URL}/#{node_path}>#{node_path}
            |] : build_summaries us
        build_summaries ( Wibble _ : us )
            = build_summaries us
    defaultLayout $ do
        common_head
        sidebar_widget (nav bliki)
        [whamlet|
<div .update_log>
    <ol .summary_listing>
        $forall summary <- update_summaries
            <li> ^{summary}     
|]

mkYesodSub "Blog" [] [parseRoutes|
/         IndexR       GET
|]

