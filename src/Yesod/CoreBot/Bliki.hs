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

mkYesodSubDispatch "Bliki" [] [parseRoutes|
/           MainR     GET
|]

mk_bliki :: Config
         -> IO Bliki
mk_bliki config = do
    {-
     - In theory Nav is not needed. Retained to document what routes were used where by nav.
    let nav = Nav.mkNav ( Text.pack in_server_name ) 
                        ( const MainR )
                        ( DataS . Bliki.EntryLatestR )
                        ( const $ BlogS Blog.IndexR )
                        ( WikiS . Wiki.IndexR )
                        ( DataS . Bliki.BlogR )
                        ( \rev_ID node_path -> DataS $ Bliki.EntryRevR rev_ID node_path)
    -}
    src_data <- Data.mk_data config
    blog <- Blog.mk_blog src_data
    wiki <- Wiki.mk_wiki src_data
    static <- Static.mk_static config
    return Bliki { data_res      = src_data
                 , blog_res      = blog
                 , wiki_res      = wiki
                 , static_res    = static
                 }

getMainR :: Yesod m => String -> GHandler Bliki m RepHtml
getMainR = do
    bliki <- getYesodSub
    config <- config $ data_res bliki
    ( layout config ) $ do
        default_blog_entry

indirect_load = do
    bliki <- getYesodSub
    config <- config $ data_res bliki
    addScript $ StaticS $ Static.FileR "main.js"
    let wiki_node_base_URL = mk_node_data_URL config
    addHamletHead [hamlet|
<script>
    \$(document).ready( function() 
    {
        \$.get ( "@{DataS Bliki.LatestR}"
              , function( data ) 
                {
                    \$(".blog_content").html(data);
                    process_HTML_for_wiki(data, $(".blog_content"), "@{wiki_node_base_URL}");
                }
              , 'html'
              );
    } );

|]

default_blog_entry = do
    indirect_load
    [whamlet|
<div .blog_content>
    Loading 
    <a href=@{DataS Bliki.LatestR}>
        latest blog entry 
    \ HTML content.
|]

