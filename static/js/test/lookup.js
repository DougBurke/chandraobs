// Query the LookUP service for converting Astronomical names to
// RA/Dec values.
//
// Switching from hard-coded success/error handling to a more-callback-based
// approach.
//
const lookup = (() => {

    var lup_httpRequest;
    
    function cleanQueryValue(val) {
	return encodeURIComponent(val)
            .replace(/!/g, '%21')
            .replace(/'/g, '%27')
            .replace(/\(/g, '%28')
            .replace(/\)/g, '%29')
            .replace(/\*/g, '%2A')
	    .replace(/%0A/g, '\n');
    }

    // Look up the name and if there is a success call
    // callback(ra, dec) and error handler (string argument,
    // representing HTML for the message).
    //
    function lookupName(objectName, callback, errhandle){

        lup_httpRequest = new XMLHttpRequest();
        if (!lup_httpRequest) {
	    errhandle("INTERNAL ERROR: unable to create request");
	    return;
        }

        var src = 'http://www.strudel.org.uk/lookUP/json/?name=' +
            cleanQueryValue(objectName);

        lup_httpRequest.onreadystatechange = nameResponse(objectName,
							  callback,
							  errhandle);
        lup_httpRequest.open('GET', src);
        lup_httpRequest.send();
    }

    function nameResponse(objectname, callback, errhandle) {

        return function(d) {
            if (lup_httpRequest.readyState === XMLHttpRequest.DONE) {
                if (lup_httpRequest.status === 200) {
                    var response = JSON.parse(lup_httpRequest.responseText);
                    processLookUP(objectname, callback, errhandle, response);
                } else {
		    errhandle('There was a problem calling the lookUP service.');
                }
            }
        }
    }

    function processLookUP(objectname, callback, errhandle, d) {

	if(typeof d=="undefined" || (d.type && d.type=="error")) {
	    errhandle('There was a problem querying the lookUP service.');
            return;
        }

	if(d.target && d.target.suggestion) {
            var msg = "<p>Target '" + objectname + "' not found.</p><p>Did you mean '" +
                d.target.suggestion + "'?</p>";
            errhandle(msg);
            return;
        }

	if(d.ra) {
	    callback(d.ra.decimal, d.dec.decimal);
            return;
        }

	var msg;
	if(d.message) {
            /* Could add objectname here, but not sure what
               LookUp is returning */
            msg = d.message;
        } else {
            msg = "Target '" + objectname + "' not found.";
        }
	errhandle(msg);
    }
    
    return { lookupName: lookupName };
    
})();
