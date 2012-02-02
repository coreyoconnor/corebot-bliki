module Yesod.CoreBot.Bliki.Base where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Resources.Base

data Bliki_ master = Bliki
    { data_res    :: Data_   master
    , blog_res    :: Blog_   master
    , wiki_res    :: Wiki_   master
    }

mkYesodSubData "Bliki_ master" [] [parseRoutes|
/           MainR     GET
|]

