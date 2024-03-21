// This used to use the lookUP service for converting Astronomical names to
// RA/Dec values -  https://www.strudel.org.uk/lookUP/ - but this service
// was shut down due to inconsiderate users.
//
// I am currently using a basic nameserver I've cobbled together. It is not
// as useful as the lookUP service.
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

        var src = 'https://cdsweb.u-strasbg.fr/cgi-bin/nph-sesame/-ox/SNV?' +
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
		try {
                  process(objectname, callback, errhandle, lup_httpRequest.responseXML);
		} catch (e) {
		  console.log(`Unable to parse nameserver response: ${e}`);
		  errhandle('Unable to decode the response from the name server.');
		}
              } else {
		errhandle('There was a problem calling the name service.');
              }
            }
        }
    }

  // Could target the Resolver element and then the ra/dec using that, but
  // for now just grab them both separately.
  //
  const raPath = "/Sesame/Target/Resolver/jradeg";
  const decPath = "/Sesame/Target/Resolver/jdedeg";

    function process(objectname, callback, errhandle, d) {

      if (typeof d === 'undefined') {
	errhandle('There was a problem querying the name server.');
	return;
      }

      // See https://developer.mozilla.org/en-US/docs/Web/XPath/Introduction_to_using_XPath_in_JavaScript
      //
      var ra = d.evaluate(raPath, d, null, XPathResult.NUMBER_TYPE, null).numberValue;
      var dec = d.evaluate(decPath, d, null, XPathResult.NUMBER_TYPE, null).numberValue;

      if (Number.isNaN(ra) || Number.isNaN(dec)) {
	errhandle(`Target "${objectname}" not found.`);
	return;
      }

      callback(ra, dec);
    }
    
    return { lookupName: lookupName };
    
})();
