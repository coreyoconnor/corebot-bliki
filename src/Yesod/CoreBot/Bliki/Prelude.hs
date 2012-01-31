module Yesod.CoreBot.Bliki.Prelude ( module Yesod.CoreBot.Bliki.Prelude
                                   , module Control.Applicative
                                   , module Control.Concurrent
                                   , module Control.Monad
                                   , module Control.Monad.Fix
                                   , module Data.Aeson
                                   , module Data.IORef
                                   , module Data.List
                                   , module Data.Maybe
                                   , module Data.Text
                                   , module System.FilePath
                                   , module Yesod
                                   )
where

import Yesod hiding ( delete
                    , deleteBy
                    , insert 
                    , insertBy
                    , get
                    )

import Blaze.ByteString.Builder ( toByteString )

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix

import Data.Aeson

import Data.IORef
import Data.List
import Data.Maybe

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Text (Text)

import Network.HTTP.Types 

import System.FilePath hiding ( joinPath )

render_absolute_URL :: RenderRoute r => Text -> r -> Text
render_absolute_URL base_URL r = 
    let route_subpath = fst $ renderRoute r
        route_sub_URL = TextEnc.decodeUtf8 $ toByteString $ encodePathSegments route_subpath
    in base_URL `Text.append` route_sub_URL

