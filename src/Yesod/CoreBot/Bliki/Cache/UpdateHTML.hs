module Yesod.CoreBot.Bliki.Cache.UpdateHTML where

import Yesod.CoreBot.Bliki.Prelude

import Yesod.CoreBot.Bliki.Config
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

import Text.Pandoc 
import Text.Pandoc.Shared

import System.Directory

pandoc_write_options :: ConfigM master m
                     => Maybe Bloggable 
                     -> m WriterOptions
pandoc_write_options mprev_update = do
    extra_vars <- case mprev_update of
                    Nothing -> return []
                    Just (UpdateBloggable _ prev_rev_ID _) -> do
                        link_URL <- revision_blog_URL prev_rev_ID
                        return $ ( "prev-blog-update"
                                 , prev_rev_ID 
                                 ) :
                                 ( "prev-blog-update-URL"
                                 , link_URL
                                 ) : []
                    Just (WikiBloggable prev_entry_path prev_rev_ID _) -> do
                        link_URL <- entry_at_rev_URL prev_entry_path prev_rev_ID
                        return $ ( "prev-node-update"
                                 , prev_entry_path
                                 ) : 
                                 ( "prev-node-update-rev"
                                 , prev_rev_ID
                                 ) :
                                 ( "prev-node-update-URL"
                                 , link_URL
                                 ) : []
    return $ defaultWriterOptions
        { writerHtml5 = True
        , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        , writerEmailObfuscation = JavascriptObfuscation
        , writerStandalone = True
        , writerVariables = extra_vars ++ writerVariables defaultWriterOptions
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
<div class="previous_update_summary">
$if(prev-node-update)$
Previously, node $prev-node-update$ was modified to revision <a href="$prev-node-update-URL$">$prev-node-update-rev$</a>
$endif$
$if(prev-blog-update)$
Previously, blog for update revision <a href="$prev-blog-update-URL$">$prev-blog-update$</a>
$endif$
</div>
|]
    }

build_blog_HTML :: ( MonadIO m, ConfigM master m )
                => Maybe Bloggable 
                -> RevisionId 
                -> String 
                -> m ()
build_blog_HTML mprev_update rev_ID txt = do
    out_path <- asks blog_HTML_path <*> pure rev_ID
    out_exists <- liftIO $ doesFileExist out_path 
    case out_exists of
        True  -> return ()
        False -> do
            liftIO $ putStrLn $ "build HTML for " ++ rev_ID ++ " log"
            let pandoc = readMarkdown defaultParserState txt
            write_opts <- pandoc_write_options mprev_update
            let html_string = writeHtmlString write_opts pandoc
            liftIO $ cache_str out_path html_string
    
build_node_HTML :: ( MonadIO m, StoreM m, ConfigM master m )
                => Maybe Bloggable 
                -> RevisionId 
                -> FilePath 
                -> m ()
build_node_HTML mprev_update rev_ID node_path = do
    out_path <- asks node_HTML_path <*> pure rev_ID <*> pure node_path
    out_exists <- liftIO $ doesFileExist out_path
    case out_exists of
        True  -> return ()
        False -> do
            liftIO $ putStrLn $ "build HTML for " ++ node_path ++ " " ++ show rev_ID
            store_data <- data_for_node_rev node_path rev_ID
            let markdown_data = FileStore.toByteString store_data
                markdown_text = TL.unpack $ TL.decodeUtf8 markdown_data
                pandoc        = readMarkdown    defaultParserState  markdown_text
            write_opts <- pandoc_write_options mprev_update
            let html_string   = writeHtmlString write_opts pandoc
            liftIO $ cache_str out_path html_string

