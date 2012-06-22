{- Widgets useful for building the wiki site. 
 - See DefaultMain.hs for an example of all of these in use at once.
 -}
module Yesod.CoreBot.Bliki.Widgets where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base
import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Resources.Base

data NavWidget master where
    NavWidget :: forall master . Yesod master => Bliki_ master -> NavWidget master

instance ToWidget sub master ( NavWidget master ) where
    toWidget (NavWidget bliki) = do
            let cfg = config $ data_res bliki
                main = main_route cfg
                blog_update_log = blog_routes cfg BlogIndexR
                wiki_index = wiki_routes cfg $ WikiIndexR []
            [whamlet|
                <ul .nav_sidebar>
                    <li>
                        <a href=@{main}>Main
                    <li><a href=@{blog_update_log}>Blog
                    <li><a href=@{wiki_index}>Wiki
            |]
            toWidget $ [cassius|            
                .nav_sidebar li
                    font-size: large;
                    list-style: none;

                .nav_sidebar
                    position: fixed;
                    margin: 0;
                    top: 0em;
                    left: 1em;
                    width: 9em;
                    height: 100%;
                    padding-top: 1em;
                    padding-right: 0.1em;
                    padding-left: 0.1em;
                    border-right: 0.3em solid #111111;

                body 
                    margin: 3em 12em;
                    padding: 0em 0.2em;

                .wiki_index li
                    list-style: none   
            |]

