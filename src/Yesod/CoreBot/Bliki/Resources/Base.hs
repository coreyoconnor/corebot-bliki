module Yesod.CoreBot.Bliki.Resources.Base ( module Yesod.CoreBot.Bliki.Resources.Base
                                          ) where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.DB
import Yesod.CoreBot.Bliki.Store

import Control.Concurrent
import Control.Monad.Reader.Class

import qualified Data.Text as Text

mkYesodSubData "Data_ master" [] [parseRoutes|
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

revision_blog_URL :: ConfigM master m
                  => RevisionId 
                  -> m String
revision_blog_URL rev_ID = do
    config :: Config master <- ask
    let base_URL = approot $ site config
        rev_blog_R = data_routes config $ BlogR rev_ID
    return $ Text.unpack $ render_absolute_URL base_URL rev_blog_R
        
    
entry_at_rev_URL :: ConfigM master m
                 => String 
                 -> RevisionId 
                 -> m String
entry_at_rev_URL entry_path rev_ID = do
    config :: Config master <- ask
    let base_URL = approot $ site config
        entry_rev_R = data_routes config $ EntryRevR rev_ID [ Text.pack entry_path ]
    return $ Text.unpack $ render_absolute_URL base_URL entry_rev_R

