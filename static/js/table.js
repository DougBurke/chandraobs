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

    // column numbers start at 0
    $("#scheduledObs").tablesorter( {
        headers: { 2: { sorter: 'extsort' },
                   3: { sorter: 'extsort' },
                   7: { sorter: 'extsort' },
                   8: { sorter: 'extsort' },
                   9: { sorter: 'extsort' }
                 }
        }
     ); 
}); 
