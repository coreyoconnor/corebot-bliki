module DefaultMain where
import Yesod

import Yesod.CoreBot.Bliki

import Paths_corebot_bliki

import Control.Monad.Fix

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

mkYesod "Main" [parseRoutes|
/       BlikiS  Bliki  bliki
/data   DataS   Data   get_data
/blog   BlogS   Blog   get_blog
/wiki   WikiS   Wiki   get_wiki
/static StaticS Static get_static
|]

main :: IO ()
main = do
    static_dir <- getDataFileName "static"
    putStrLn $ "using static dir: " ++ static_dir
    app <- mfix $ \app -> do
                let config = Config { store_dir = "/home/coconnor/bliki"
                                    , cache_dir = "/home/coconnor/bliki/cache"
                                    -- XXX: I don't think building this table is required.
{-
                                    , data_routes = DataRoutes
                                        { latest_R     = DataS LatestR
                                        , update_log_R = DataS UpdateLogR
                                        , entry_latest_R = DataS EntryLatestR
                                        , blog_R = DataS BlogR
                                        , entry_rev_R = DataS EntryRevR
                                        }
                                    , blog_routes = BlogRoutes
                                        { blog_index_R = BlogS BlogIndexR }
                                    , wiki_routes = WikiRoutes
                                        { wiki_index_R = WikiS WikiIndexR }
                                    , static_routes = StaticRoutes
                                        { file_R = StaticS FileR }
                                    , static_config = UseDir static_dir
-}
                                    }
                bliki <- mk_bliki config
                return $ Main bliki
    warpDebug 8080 app

