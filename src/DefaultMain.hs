{-# LANGUAGE CPP #-}
#ifdef CABAL_EXE_BUILD
module Main where
#else
module DefaultMain where
#endif
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
    { bliki :: Bliki_ Main
    }

type Bliki = Bliki_ Main

type Data = Data_ Main
get_data = data_res . bliki

type Blog = Blog_ Main
get_blog = blog_res . bliki

type Wiki = Wiki_ Main
get_wiki = wiki_res . bliki

get_static = static_config . config . data_res . bliki

instance Yesod Main where
    approot _ = "http://localhost:8080"
    defaultLayout w = do
        bliki <- bliki <$> getYesod
        let nav = toWidget $ NavWidget bliki
        let page_w = mconcat [ nav, w ]
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
    defaultLayout $ do
        default_blog_entry bliki

mkYesod "Main" [parseRoutes|
/       MainR   GET
/data   DataS   Data   get_data
/blog   BlogS   Blog   get_blog
/wiki   WikiS   Wiki   get_wiki
/static StaticS Static get_static
|]

main :: IO ()
main = do
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
                                    , site = app
                                    }
                bliki <- mk_bliki config
                return $ Main bliki
    warpDebug 8080 app

