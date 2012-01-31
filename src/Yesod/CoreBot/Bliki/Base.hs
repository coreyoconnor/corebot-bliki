module Yesod.CoreBot.Bliki.Base where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Resources.Base

data Bliki = Bliki
    { data_res        :: Data
    , blog_ref        :: Blog
    , wiki_res        :: Wiki
    , static_res      :: Static
    }

mkYesodSubData "Bliki" [] [parseRoutes|
/           MainR     GET
|]

