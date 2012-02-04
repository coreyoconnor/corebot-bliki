{- Widgets useful for building the wiki site. 
 - See DefaultMain.hs for an example of all of these in use at once.
 -}
module Yesod.CoreBot.Bliki.Widgets where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base
import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Resources.Base

data NavWidget = NavWidget

instance ToWidget sub master NavWidget where
    toWidget _ = do
        return ()

