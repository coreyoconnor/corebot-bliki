module Yesod.CoreBot.Bliki.Resources.Static where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Base
import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Resources.Base

import System.Directory ( doesFileExist )

getFileR :: Yesod m => String -> GHandler Static m ()
getFileR file_name = do
    mode <- getYesodSub
    case mode of
        UseServer server -> fail "XXX: UseServer server should not be hit"
        UseDir dir -> do
            let file_path = dir </> file_name
                file_type = case takeExtension file_path of
                                ".js" -> typeJavascript
                                _     -> typePlain
            exists <- liftIO $ doesFileExist file_path
            case exists of
                True  -> sendFile file_type file_path
                False -> notFound
    
mkYesodSubDispatch "Static" [] [parseRoutes|
/#String  FileR GET
|]

