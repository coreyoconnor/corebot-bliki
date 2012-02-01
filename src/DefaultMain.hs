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

instance Yesod Main where
    approot _ = "http://localhost:8080"

mkYesod "Main" [parseRoutes|
/       BlikiS Bliki bliki
/data   DataS  Data  get_data
/blog   BlogS  Blog  get_blog
/wiki   WikiS  Wiki  get_wiki
|]

main :: IO ()
main = do
    app <- mfix $ \app -> do
                let config = Config { store_dir = "/home/coconnor/bliki"
                                    , cache_dir = "/home/coconnor/bliki/cache"
                                    -- XXX: I don't think building this table is required.
                                    , data_routes = DataRoutes
                                        { latest_R     = DataS LatestR
                                        , update_log_R = DataS UpdateLogR
                                        , 
                                    }
                bliki <- mk_bliki config
                return $ Main bliki
    warpDebug 8080 app

