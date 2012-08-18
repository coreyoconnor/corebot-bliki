module Yesod.CoreBot.Bliki.Cache.UpdateHTML where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
import Yesod.CoreBot.Bliki.Resources.Base
import Yesod.CoreBot.Bliki.Store
import Yesod.CoreBot.Bliki.DB

import HereDoc

import Control.Monad.Reader

import qualified Data.ByteString.Lazy as B

import Data.FileStore ( Revision(..) 
                      , TimeRange(..)
                      , Resource
                      , RevisionId
                      )
import qualified Data.FileStore as FileStore

import qualified Data.Text as Text

import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Text.Blaze.Renderer.String as HTMLRenderer
import Text.Pandoc 
import Text.Pandoc.Shared

import System.Directory

pandoc_write_options :: ConfigM master m
                     => m WriterOptions
pandoc_write_options = do
    return $ defaultWriterOptions
        { writerHtml5 = True
        , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        , writerEmailObfuscation = JavascriptObfuscation
        , writerStandalone = True
        , writerTemplate = [heredoc|
$for(include-before)$
$include-before$
$endfor$
$if(title)$
<header>
<h1 class="title">$title$</h1>
$for(author)$
<h2 class="author">$author$</h2>
$endfor$
$if(date)$
<h3 class="date">$date$</h3>
$endif$
</header>
$endif$
$if(toc)$
<nav id="$idprefix$TOC">
$toc$
</nav>
$endif$
$body$
$for(include-after)$
$include-after$
$endfor$
|]
    }

build_blog_HTML :: ( MonadIO m, ConfigM master m )
                => Maybe Bloggable 
                -> RevisionId 
                -> String 
                -> m ()
build_blog_HTML mprev_update rev_ID txt = 
    if_missingM (asks blog_HTML_path <*> pure rev_ID) $ \out_path -> do
        liftIO $ putStrLn $ "build HTML for " ++ rev_ID ++ " log"
        let pandoc = readMarkdown defaultParserState txt
        write_opts <- pandoc_write_options
        let html_string = writeHtmlString write_opts pandoc
        liftIO $ cache_str out_path html_string

build_node_HTML :: forall m master . ( MonadIO m, StoreM m, ConfigM master m )
                => Maybe Bloggable 
                -> RevisionId 
                -> FilePath 
                -> m ()
build_node_HTML mprev_update rev_ID node_path = do
    if_missingM (asks node_HTML_path <*> pure rev_ID <*> pure node_path) $ \out_path -> do
        liftIO $ putStrLn $ "build HTML for " ++ node_path ++ " " ++ show rev_ID
        store_data <- data_for_node_rev node_path rev_ID
        let markdown_data = FileStore.toByteString store_data
            markdown_text = TL.unpack $ TL.decodeUtf8 markdown_data
            pandoc        = readMarkdown defaultParserState markdown_text
        write_opts <- pandoc_write_options
        let body_html_str = writeHtmlString write_opts pandoc
        let html_builder :: ( Route master -> [(Text, Text)] -> Text ) -> Html = [hamlet|
        #{preEscapedToMarkup body_html_str}
        |]
        html_str <- HTMLRenderer.renderHtml <$> ( pure html_builder <*> asks route_render )
        liftIO $ cache_str out_path html_str

if_missingM :: ( MonadIO m ) 
            => m FilePath 
            -> ( FilePath -> m () )
            -> m ()
if_missingM pM aM = do
    path <- pM
    exists <- liftIO $ doesFileExist path
    case exists of
        True  -> return ()
        False -> aM path
    
