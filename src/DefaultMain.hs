module DefaultMain where
import Yesod

import Yesod.CoreBot.Bliki

import Paths_corebot_bliki

main :: IO ()
main = do
    bliki <- mkBliki "http://localhost:8080" 
                     "/home/coconnor/Development/corebotllc.com/static"
                     "/home/coconnor/bliki"
                     "/home/coconnor/bliki/cache"
    warpDebug 8080 bliki

