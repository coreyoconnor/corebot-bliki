module DefaultMain where
import Yesod

import Yesod.CoreBot.Bliki

import Paths_corebot_bliki

data Main = Main
    { bliki :: Bliki_ Main
    }

type Bliki = Bliki_ Main
type Data = Data_ Main
type Blog = Blog_ Main
type Wiki = Wiki_ Main

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
|]
