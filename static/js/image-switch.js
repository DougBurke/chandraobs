/*
 * Simple attempt to support user selection of the DSS/PSPC/RASS links.
 */

"use strict";

var imageSwitch = (function() {

    /* For now hard-code the names */
    var viewElems = ['Chandra', 'DSS', 'RASS', 'PSPC', 'Details', 'WWT'];

    function switchOption(name) {

        for (const oname of viewElems) {
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
        
        // Special-case the WWT handling
        if (name === 'WWT') {
            wwt.setLocation(scienceObs);
        }
    }

    return {switchOption: switchOption};

})();
