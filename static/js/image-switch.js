/*
 * Simple attempt to support user selection of the DSS/PSPC/RASS links.
 */

var imageSwitch = (function() {

/* For now hard-code the names */
var viewElems = ['Chandra', 'DSS', 'RASS', 'PSPC', 'Details'];

function switchOption(name) {

    var n = viewElems.length;
    for (var i = 0; i < n; i++) {
        var oname = viewElems[i];
        var elem = document.getElementById(oname);
        if (elem === null) {
            continue;
        }

        if (oname === name) {
            elem.className = 'active';
        } else {
            elem.className = 'inactive';
        }
    }
    
    // This was needed due to a move to AJAX-style interface,
    // but it's not clear it is still needed.
    document.getElementById(name + 'button').checked = true;
}

    return {switchOption: switchOption};

})();
