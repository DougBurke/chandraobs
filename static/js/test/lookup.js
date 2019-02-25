// Query the LookUP service for converting Astronomical names to
// RA/Dec values.
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

    function lookupName(objectName){

        lup_httpRequest = new XMLHttpRequest();
        if (!lup_httpRequest) {
	    console.log("INTERNAL ERROR: unable to create request");
	    return;
        }

        var src = 'http://www.strudel.org.uk/lookUP/json/?name=' +
            cleanQueryValue(objectName);

        lup_httpRequest.onreadystatechange = nameResponse(objectName);
        lup_httpRequest.open('GET', src);
        lup_httpRequest.send();
    }

    function nameResponse(objectname) {

        return function(d) {
            if (lup_httpRequest.readyState === XMLHttpRequest.DONE) {
                if (lup_httpRequest.status === 200) {
                    var response = JSON.parse(lup_httpRequest.responseText);
                    processLookUP(objectname, response);
                } else {
                    reportLookupFailure('There was a problem calling the lookUP service.');
                }
            }
        }
    }

    function processLookUP(objectname, d) {

	if(typeof d=="undefined" || (d.type && d.type=="error")) {
            reportLookupFailure('There was a problem querying the lookUP service.');
            return;
        }

	if(d.target && d.target.suggestion) {
            var msg = "<p>Target '" + objectname + "' not found.</p><p>Did you mean '" +
                d.target.suggestion + "'?</p>";
            reportLookupFailure(msg);
            return;
        }

	if(d.ra) {
	    // TODO: should pass around the WWT object in the lookup call
	    const wwt = main.getWWT();
	    const zoom = wwt.get_fov();
	    wwt.gotoRaDecZoom(d.ra.decimal, d.dec.decimal, zoom);
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
        reportLookupFailure(msg);
    }
    
    function reportLookupSuccess(msg, close) {
	const host = main.getHost();
	if (host === null) {
	    return;
	}
	
	const pane = document.createElement('div');
	pane.setAttribute('class', 'lookup-success');
	pane.innerHTML = msg;

	host.appendChild(pane);
	
	if (typeof close === 'undefined') { close = true; }
        if (close) {
	    // hide the pane in 4 seconds
            window.setTimeout(() => pane.style.display = 'none',
			      4000);
        }

    }

    function reportLookupFailure(msg, close) {
	const host = main.getHost();
	if (host === null) {
	    return;
	}
	
	const pane = document.createElement('div');
	pane.setAttribute('class', 'lookup-failure');
        pane.innerHTML = msg;
	/***
            '<button type="button" class="close" ' +
            'onclick="wwt.hideElement(' + "'targetFailure'" +
            '); wwt.setTargetName(' + "''" + ');">X</button><div>' +
            msg + '</div>';
	***/
	
	host.appendChild(pane);

	if (typeof close === 'undefined') { close = true; }
        if (close) {
	    // hide the pane in 4 seconds
            window.setTimeout(() => pane.style.display = 'none',
			      4000);
        }
	
    }

    return { lookupName: lookupName };
    
})();
