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
            main_URL <- approot <$> ( lift $ getYesod )
            let cfg = config $ data_res bliki
                blog_update_log = blog_routes cfg BlogIndexR
                wiki_index = wiki_routes cfg $ WikiIndexR []
            [whamlet|
                <ul .nav_sidebar>
                    <li>
                        <a href=#{main_URL}>Main
                    <li><a href=@{blog_update_log}>Blog
                    <li><a href=@{wiki_index}>Wiki
            |]

