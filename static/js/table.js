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

        $("#scheduledObs").tablesorter( {
            headers: { 6: { sorter: 'extsort' },
                       7: { sorter: 'extsort' },
                       9: { sorter: 'extsort' },
                       10: { sorter: 'extsort' }
                     }
            }
         ); 
}); 
