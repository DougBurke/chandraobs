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
            headers: { 2: { sorter: 'extsort' },
                       3: { sorter: 'extsort' },
                       5: { sorter: 'extsort' },
                       6: { sorter: 'extsort' }
                     }
            }
         ); 
}); 
