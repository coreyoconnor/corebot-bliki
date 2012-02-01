module Yesod.CoreBot.Bliki.Base where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Resources.Base

data Bliki master = Bliki
    { data_res  :: Data master
    , blog_res  :: Blog master
    , wiki_res  :: Wiki master
    }

mkYesodSubData "Bliki master" [] [parseRoutes|
/           MainR     GET
|]

