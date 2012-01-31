module Yesod.CoreBot.Bliki where

import Yesod.CoreBot.Bliki.Base

import Yesod.CoreBot.Bliki.Widget.Head

import Yesod.CoreBot.Bliki.Resources.Blog ( Blog )
import qualified Yesod.CoreBot.Bliki.Resources.Blog as BlogRes

import Yesod.CoreBot.Bliki.Resources.Data ( Data )
import qualified Yesod.CoreBot.Bliki.Resources.Data as DataRes

import Yesod.CoreBot.Bliki.Resources.Static ( Static )
import qualified Yesod.CoreBot.Bliki.Resources.Static as StaticRes

import Yesod.CoreBot.Bliki.Resources.Wiki ( Wiki )
import qualified Yesod.CoreBot.Bliki.Resources.Wiki as WikiRes

import qualified Data.Text as Text
import qualified Data.Text.Encoding

mkYesodSubDispatch "Bliki" bliki_resources

mkBliki :: String 
        -> String 
        -> FilePath
        -> FilePath
        -> IO Bliki
mkBliki in_server_name 
        in_static_base_URL 
        in_bliki_dir
        in_cache_dir
    = do
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
    src_data <- DataRes.mkBliki in_bliki_dir
                           in_cache_dir
                           nav
    blog <- Blog.mkBlog bliki
    wiki <- Wiki.mkWiki bliki
    static <- Static.mk_static in_static_base_URL
    return CoreBotWWW { server_name     = in_server_name
                      , static_base_URL = in_static_base_URL
                      , data_route      = bliki
                      , blog_route      = blog
                      , wiki_route      = wiki
                      , nav             = nav
                      , static_route    = static
                      }

getMainR :: Handler RepHtml
getMainR = do
    corebot <- getYesod
    defaultLayout $ do
        setTitle "CoreBot - Home"
        default_head corebot
        Nav.sidebar_widget $ nav corebot
        default_blog_entry

default_head corebot = do
    common_head
    addScript $ StaticS $ Static.FileR "main.js"
    let wiki_node_base_URL = DataS $ Bliki.EntryLatestR []
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

default_blog_entry = [whamlet|
<div .blog_content>
    Loading 
    <a href=@{DataS Bliki.LatestR}>
        latest blog entry 
    \ HTML content.
|]

