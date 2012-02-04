module Yesod.CoreBot.Bliki.Base where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Resources.Base

data Bliki_ master = Bliki
    { data_res    :: Data_   master
    , blog_res    :: Blog_   master
    , wiki_res    :: Wiki_   master
    }

-- | The bliki can also be used as a subsite. If so then these are the routes supported by the
-- Bliki_ subsite.
mkYesodSubData "Bliki_ master" [] [parseRoutes|
/           MainR     GET
|]

