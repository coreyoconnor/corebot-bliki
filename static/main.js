var worker = new Worker('static/process_HTML_for_wiki.js');

function insertSubstr( str, index, sub_str )
{
    var str_pre = str.slice( 0, index );
    var str_post = str.slice( index );
    return str_pre + sub_str + str_post;
}

function process_HTML_for_wiki( data, target, wiki_node_URL )
{
    var edits = new Array;

    var apply_edits = function()
    {
        edits.reverse();

        for(var i = 0; i < edits.length ; ++i)
        {
            var edit = edits[i];
            var edit_region = edit.edit_region;
            // +- 2 accounts for the ]] and [[ respectively. 
            // XXX: hackish
            data = insertSubstr( data, edit_region.end_index + 2, '</a>' );
            data = insertSubstr( data, edit_region.start_index - 2, '<a href="' + edit.node_URL + '">' );
        }

        target.html( data );
    }

    worker.postMessage( data );
    worker.onmessage = function(event)
        {
            edit_region = event.data;

            var node_URL = wiki_node_URL + '/' + edit_region.node_path;

            var http = new XMLHttpRequest();
            http.open('HEAD', node_URL, true);
            http.onload = function( pe )
                {
                    console.log( http.status );

                    // Thanks the gods of HTTP for making this next line so.
                    if( http.status < 400 )
                    {
                        edits.push( { edit_region: edit_region
                                    , node_URL: node_URL
                                    }
                                  );
                        
                        if( edit_region.is_final )
                            apply_edits();
                    }
                };
            http.send();
        };
}

