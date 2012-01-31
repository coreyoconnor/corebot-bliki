module Yesod.CoreBot.Bliki.Widget.Head where

import Yesod.CoreBot.Bliki.Prelude

common_head = do
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"

