module CoreBot where

import Yesod

import Yesod.CoreBot.Bliki 
import Yesod.CoreBot.Bliki.Widgets

import Paths_corebot_bliki

import Control.Applicative
import Control.Monad.Fix

import Data.Monoid 

import System.Directory ( getHomeDirectory )

import System.FilePath

data Main = Main
    { bliki :: Bliki
    , root_URL :: String
    }

instance HasBliki Main where
  get_bliki = bliki

instance Yesod Main where
    approot = ApprootMaster root_URL
    defaultLayout w = do
        bliki <- bliki <$> getYesod
        let page_w = mconcat [ nav_widget bliki, w ]
        p <- widgetToPageContent page_w
        mmsg <- getMessage
        hamletToRepHtml [hamlet|
!!!

<html>
    <head>
        <title>#{pageTitle p}
        ^{pageHead p}
    <body>
        $maybe msg <- mmsg
            <p .message>#{msg}
        ^{pageBody p}
|]

getMainR = do
    bliki <- bliki <$> getYesod
    defaultLayout [whamlet|
^{default_blog_entry bliki}
|]

mkYesod "Main" [parseRoutes|
/                             MainR         GET
/blog                         BlogIndexR    GET
/data                         UpdateLogR    GET
/data/latest                  LatestR       GET
/data/entry/*Texts            EntryLatestR  GET
/data/blog/#RevisionId        BlogR         GET
/data/rev/#RevisionId/*Texts  EntryRevR     GET
/wiki/*Texts                  WikiIndexR    GET
/static/#String               FileR         GET
|]

main root_URL store_dir cache_dir = do
    home_dir <- getHomeDirectory
    let store_dir = home_dir </> "bliki"
        cache_dir = home_dir </> "bliki/cache"
    static_dir <- getDataFileName "static"
    putStrLn $ "using static dir: " ++ static_dir
    app <- mfix $ \app -> do
                let config = Config { store_dir     = store_dir
                                    , cache_dir     = cache_dir
                                    , data_routes   = DataS
                                    , blog_routes   = BlogS
                                    , wiki_routes   = WikiS
                                    , static_routes = StaticS
                                    , static_config = UseDir static_dir
                                    , route_render  = yesodRender app root_URL
                                    , probe_period  = 10
                                    }
                bliki <- mk_bliki config
                return $ Main bliki
    warpDebug 8080 app

