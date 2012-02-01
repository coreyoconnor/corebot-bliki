module Yesod.CoreBot.Bliki.Resources.Base ( module Yesod.CoreBot.Bliki.Resources.Base
                                          ) where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.DB
import Yesod.CoreBot.Bliki.Store

import Control.Concurrent

import Language.Haskell.TH.Syntax

data Data master = Data 
    { config             :: Config master
    , store              :: Store
    , update_thread_ID   :: ThreadId
    , db_ref             :: IORef DB
    }

data Blog master = Blog ( Data master )

data Wiki master = Wiki ( Data master )

mkYesodSubData "Data master" [ ] [parseRoutes|
/latest                     LatestR      GET
/                           UpdateLogR   GET
/entry/*Texts               EntryLatestR GET
/blog/#RevisionId           BlogR        GET 
/rev/#RevisionId/*Texts     EntryRevR    GET
|]

mkYesodSubData "Blog master" [] [parseRoutes|
/         BlogIndexR       GET
|]

mkYesodSubData "Wiki master" [] [parseRoutes|
/*Texts  WikiIndexR GET
|]

mkYesodSubData "Static" [] [parseRoutes|
/#String  FileR GET
|]
