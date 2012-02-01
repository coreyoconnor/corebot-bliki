module DefaultMain where
import Yesod

import Yesod.CoreBot.Bliki

import Paths_corebot_bliki

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

main :: IO ()
main = do
    config :: Config Main <- mk_config  "/home/coconnor/bliki" 
                                        "/home/coconnor/bliki/cache"
    bliki <- mk_bliki config
    warpDebug 8080 ( Main bliki )

mkYesod "Main" [parseRoutes|
/       BlikiS Bliki bliki
/data   DataS  Data  get_data
/blog   BlogS  Blog  get_blog
/wiki   WikiS  Wiki  get_wiki
|]
