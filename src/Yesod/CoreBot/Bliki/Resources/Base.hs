module Yesod.CoreBot.Bliki.Resources.Base ( module Yesod.CoreBot.Bliki.Resources.Base
                                          ) where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.DB
import Yesod.CoreBot.Bliki.Store

import Control.Concurrent

import Language.Haskell.TH.Syntax

mkYesodSubData "Data_ master" [ ] [parseRoutes|
/latest                     LatestR      GET
/                           UpdateLogR   GET
/entry/*Texts               EntryLatestR GET
/blog/#RevisionId           BlogR        GET 
/rev/#RevisionId/*Texts     EntryRevR    GET
|]

mkYesodSubData "Blog_ master" [] [parseRoutes|
/         BlogIndexR       GET
|]

mkYesodSubData "Wiki_ master" [] [parseRoutes|
/*Texts  WikiIndexR GET
|]

mkYesodSubData "Static" [] [parseRoutes|
/#String  FileR GET
|]
