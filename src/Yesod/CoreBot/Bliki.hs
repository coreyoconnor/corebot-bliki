module Yesod.CoreBot.Bliki ( mk_bliki
                           , default_blog_entry
                           , indirect_load
                           , Bliki_(..)
                           , module Yesod.CoreBot.Bliki.Config
                           , module Yesod.CoreBot.Bliki.Resources.Base
                           ) where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base
import Yesod.CoreBot.Bliki.Config

import Yesod.CoreBot.Bliki.Resources.Base
import qualified Yesod.CoreBot.Bliki.Resources.Blog as Blog
import qualified Yesod.CoreBot.Bliki.Resources.Data as Data
import qualified Yesod.CoreBot.Bliki.Resources.Static as Static
import qualified Yesod.CoreBot.Bliki.Resources.Wiki as Wiki

import qualified Data.Text as Text
import qualified Data.Text.Encoding

-- | Constructs a Bliki component given the config.
--
-- This is parameterised by the Yesod app. Represented by the "master" type variable. With this type
-- and the routes in Config the Bliki can coordinate between other subsites of the app. The other
-- subsites support the resources required by the bliki. 
--
-- This would not need the routes in Config and the master type if a Bliki was entirely represented
-- by a single Yesod subsite. I did not find that this provided the flexibility I wanted to
-- supported both the the REST URIs for corebotllc.com and the URIs other people might want.
--
-- For an example of how a Bliki is constructed and integrated into a Yesod app see DefaultMain.hs
mk_bliki :: Yesod master
         => Config master
         -> IO ( Bliki_ master )
mk_bliki config = do
    src_data <- Data.mk_data config
    blog <- Blog.mk_blog src_data
    wiki <- Wiki.mk_wiki src_data
    return Bliki { data_res      = src_data
                 , blog_res      = blog
                 , wiki_res      = wiki
                 }

getMainR = do
    bliki <- getYesodSub
    defaultLayout $ do
        default_blog_entry bliki

indirect_load bliki data_R = do
    let cfg = config $ data_res bliki
    let wiki_node_R = data_routes cfg $ EntryLatestR []
    addScript $ static_routes cfg $ FileR "jquery.min.js"
    addScript $ static_routes cfg $ FileR "indirect_load.js"
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
                    process_HTML_for_wiki(data, $(".blog_content"), "@{wiki_node_R}");
                }
              , 'html'
              );
    } );

|]

default_blog_entry bliki = do
    let cfg = config $ data_res bliki
        data_R = data_routes cfg $ LatestR
    indirect_load bliki data_R
    [whamlet|
<div .blog_content>
    Loading 
    <a href=@{data_R}>
        latest blog entry 
    \ HTML content.
|]

mkYesodSubDispatch "Bliki_ master" [] [parseRoutes|
/           MainR     GET
|]

