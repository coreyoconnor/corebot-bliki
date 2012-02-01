module Yesod.CoreBot.Bliki where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base
import Yesod.CoreBot.Bliki.Config

import Yesod.CoreBot.Bliki.Widget.Head

import Yesod.CoreBot.Bliki.Resources.Base
import qualified Yesod.CoreBot.Bliki.Resources.Blog as Blog
import qualified Yesod.CoreBot.Bliki.Resources.Data as Data
import qualified Yesod.CoreBot.Bliki.Resources.Static as Static
import qualified Yesod.CoreBot.Bliki.Resources.Wiki as Wiki

import qualified Data.Text as Text
import qualified Data.Text.Encoding

mkYesodSubDispatch "Bliki master" [] [parseRoutes|
/           MainR     GET
|]

mk_bliki :: Yesod master 
         => Config master
         -> IO ( Bliki master )
mk_bliki config = do
    src_data <- Data.mk_data config
    blog <- Blog.mk_blog src_data
    wiki <- Wiki.mk_wiki src_data
    return Bliki { data_res      = src_data
                 , blog_res      = blog
                 , wiki_res      = wiki
                 }

getMainR :: Yesod m => GHandler ( Bliki m ) m RepHtml
getMainR = do
    bliki <- getYesodSub
    ( layout $ config $ data_res bliki ) $ do
        default_blog_entry

indirect_load data_R = do
    bliki <- lift $ getYesodSub
    let cfg = config $ data_res bliki
    let base_R = entry_latest_R ( data_routes cfg )
    -- XXX: the $(.blog_content) is not specific enough. Needs to be exactly the element tied to
    -- this data_R
    addHamletHead [hamlet|
<script>
    \$(document).ready( function() 
    {
        \$.get ( "@{data_R}"
              , function( data ) 
                {
                    \$(".blog_content").html(data);
                    process_HTML_for_wiki(data, $(".blog_content"), "@{base_R}");
                }
              , 'html'
              );
    } );

|]

default_blog_entry = do
    bliki <- lift $ getYesodSub
    let cfg = config $ data_res bliki
        latest_data_R = latest_route ( data_routes cfg )
    indirect_load latest_data_R
    [whamlet|
<div .blog_content>
    Loading 
    <a href=@{latest_data_R}>
        latest blog entry 
    \ HTML content.
|]

