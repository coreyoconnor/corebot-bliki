module Yesod.CoreBot.Bliki.Resources.Blog where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Resources.Base

import Yesod.CoreBot.Bliki.Config 
import Yesod.CoreBot.Bliki.DB 
import Yesod.CoreBot.Bliki.Store 

import Yesod.CoreBot.Bliki.Widget.Head

import qualified Data.Text as Text

mk_blog :: Data_ master -> IO ( Blog_ master )
mk_blog src_data = return $ Blog src_data 

-- XXX: needs json representation
getBlogIndexR :: Yesod master => GHandler ( Blog_ master ) master RepHtml
getBlogIndexR = do
    blog@(Blog src_data) <- getYesodSub
    db <- liftIO $ readIORef $ db_ref src_data
    let updates = take 50 $ update_log db
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
            let node_R = entry_latest_R $ data_routes (config src_data) 
            in [whamlet|
                <p .node_update>
                    Added 
                    <a href=@{node_R}/#{node_path}>#{node_path}
            |] : build_summaries us
        build_summaries ( EntryChanged _ node_path : us ) = 
            let node_R = entry_latest_R $ data_routes (config src_data)
            in [whamlet|
                <p .node_update>
                    Changed 
                    <a href=@{node_R}/#{node_path}>#{node_path}
            |] : build_summaries us
        build_summaries ( Wibble _ : us )
            = build_summaries us
    defaultLayout $ do
        [whamlet|
<div .update_log>
    <ol .summary_listing>
        $forall summary <- update_summaries
            <li> ^{summary}     
|]

mkYesodSubDispatch "Blog_ master" [] [parseRoutes|
/         BlogIndexR       GET
|]

