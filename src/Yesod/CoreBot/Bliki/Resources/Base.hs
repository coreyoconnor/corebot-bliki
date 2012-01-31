module Yesod.CoreBot.Bliki.Resources.Base ( module Yesod.CoreBot.Bliki.Resources.Base
                                          ) where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.DB
import Yesod.CoreBot.Bliki.Store

import Control.Concurrent

data Data = Data 
    { config             :: Config
    , store              :: Store
    , update_thread_ID   :: ThreadId
    , db_ref             :: IORef DB
    }

data Blog = Blog Data

data Wiki = Wiki Data

data Static 
    = UseServer String
    | UseDir FilePath

mkYesodSubData "Data" [] [parseRoutes|
/latest                     LatestR      GET
/                           UpdateLogR   GET
/entry/*Texts               EntryLatestR GET
/blog/#RevisionId           BlogR        GET 
/rev/#RevisionId/*Texts     EntryRevR    GET
|]

mkYesodSubData "Blog" [] [parseRoutes|
/         BlogIndexR       GET
|]

mkYesodSubData "Wiki" [] [parseRoutes|
/*Texts  WikiIndexR GET
|]

mkYesodSubData "Static" [] [parseRoutes|
/#String  FileR GET
|]

