module Yesod.CoreBot.Bliki.Base where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base

data Bliki = Bliki
    { data_res        :: Data
    , blog_ref        :: Blog
    , wiki_res        :: Wiki
    , nav             :: Nav
    , static_res      :: Static
    }

bliki_resources = [parseRoutes|
/           MainR     GET
/data       DataS     Data   data_route
/blog       BlogS     Blog   blog_route
/wiki       WikiS     Wiki   wiki_route
/static     StaticS   Static static_route
|]

mkYesodSubData "Bliki" bliki_resources

