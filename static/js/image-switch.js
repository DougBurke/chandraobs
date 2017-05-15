/*
 * Simple attempt to support user selection of the DSS/PSPC/RASS links.
 * The selection is retained as a user switches observations; this
 * may or may-not be sensible.
 */

/*

The addition of including Chandra data makes this awkward, as there's
no guarantee that it exists.

var selectedOption = 'DSS';
*/

var selectedOption;

function switchOption(name) {
    name = (name === undefined) ? selectedOption : name;

    if (selectedOption === undefined) {
        /* if there's no selectedOption, they make everything inactive;
           this hard-codes the elements. It really should be done by
           looping over the document structure.
        */
        var elems = ['Chandra', 'DSS', 'RASS', 'PSPC', 'Details'];
        var n = elems.length;
        for (var i = 0; i < n; i++) {
            var oname = elems[i];
            if (oname !== name) {
                var elem = document.getElementById(oname);
                if (elem !== null) {
                    elem.className = 'inactive';
                }
            }
        }
    } else {
	document.getElementById(selectedOption).className = 'inactive';
    }
    
    document.getElementById(name).className = 'active';
    
    // it's not clear why this needs to be done; needed due to
    // move to AJAX-style interface
    document.getElementById(name + 'button').checked = true;
    selectedOption = name;
}

