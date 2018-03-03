
"use strict";

$(document).ready(function() { 

    $.tablesorter.addParser({
        id: 'extsort',
        is: function (s) {
            return false; // this parser is not auto-detected
        },
        format: function (s, table, cell, cellIndex) {
            return $(cell).attr('data-sortvalue');
        },
        type: 'numeric'
    });

    // Is this sensible?
    $.tablesorter.addParser({
        id: 'extsortText',
        is: function (s) {
            return false; // this parser is not auto-detected
        },
        format: function (s, table, cell, cellIndex) {
            return $(cell).attr('data-sortvalue');
        },
        type: 'text'
    });

    // column numbers start at 0
    $("#scheduledObs").tablesorter( {
        headers: {
            0: { sorter: 'extsortText' },
            2: { sorter: 'extsort' },
            3: { sorter: 'extsort' },
            7: { sorter: 'extsort' },
            8: { sorter: 'extsort' },
            9: { sorter: 'extsort' }
        }
    }
     ); 
}); 
