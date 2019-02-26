// Experiment with a WWT-based main display
//

const main = (function() {

    // How are FOVs to be drawn (for the "not-selected" case)
    //
    const fovColor = 'gray';
    const fovLineWidth = 2;
    const fovOpacity = 0.5;

    const fovSelectedColor = 'gold';
    const fovSelectedLineWidth = 4;
    const fovSelectedOpacity = 1;

    var selectedFOV = null;
    
    // Field size when zooming to a location.
    //
    const defaultFieldSize = 2.0;
    
    var wwt;

    // Return the WWT div, or null (which indicates a serious problem).
    //
    function getHost() {
	const host = document.querySelector('#WorldWideTelescopeControlHost');
	if (host === null) {
	    console.log('INTERNAL ERROR: #WorldWideTelescopeControlHost not found!');
	}
	return host;
    }
    
    var noServerPara =
        '<p class="noserver">' +
        'It looks like the web server has shut ' +
        'down. Try re-loading the page, as it ' +
        'may be due to the Heroku application ' +
        'shutting down. If this is the case it ' +
        'could take a short time for things to come ' +
        'back on-line.' +
        '</p>';
    
    function serverGoneByBy() {
	const host = getHost();
	if (host === null) {
	    alert("Things are not as I expect them to be.\nAll I can suggest doing is re-loading the page.");
	    return;
	}

	const div = document.createElement('div');
	div.setAttribute('class', 'server-problem');

	div.innerHTML = noServerPara;
	host.appendChild(div);
    }

    // Sent the div for the whole pain
    function addCloseButton(parent) {
	const el = document.createElement('span');
	el.setAttribute('class', 'closable');
	el.addEventListener('click', () => {
	    parent.style.display = 'none';
	    resetFOV();
	});
	return el;
    }

    // Sent the div for the main contents (but not the controlElements
    // bar).
    //
    function addHideShowButton(parent) {
	const el = document.createElement('span');
	el.classList.add('switchable');
	el.classList.add('hideable');
	el.addEventListener('click', () => {
	    if (el.classList.contains('hideable')) {
		parent.style.display = 'none';
		el.classList.remove('hideable');
		el.classList.add('showable');
	    } else {
		parent.style.display = 'block';
		el.classList.remove('showable');
		el.classList.add('hideable');
	    }
	});
	return el;
    }

    // Create a li element labelled by label that changes the display to the contents of
    // rsp[field], and the div with this content, with a class of
    // 'obs-' + field
    //
    // All content is hidden. It is expected that the li element for the status/observation
    // details is "clicked" to toggle the display correctly after all elements have been
    // added.
    //
    function make_display_elements(parent, rsp, field, label) {

	const txtName = 'obs-' + field;
	const liName = 'sel-' + field;
	
	const div = document.createElement('div');
	div.setAttribute('class', txtName);
	div.innerHTML = rsp[field];
	div.style.display = 'none';
	
	const li = document.createElement('li');
	li.setAttribute('class', liName);

	const a = document.createElement('a');
	a.setAttribute('href', '#');
	a.textContent = label;
	a.addEventListener('click', () => {

	    parent.querySelectorAll('div.content > div')
		.forEach(el => el.style.display = 'none');

            parent.querySelectorAll('ul.sections > li')
		.forEach(el => el.classList.remove('active'));
	
	    div.style.display = 'block';
	    li.classList.add('active');
	});

	li.appendChild(a);
	
	return {li: li, content: div, a: a};
    }

    // Reset the FOV properties to their "unselected" values.
    //
    function resetFOV() {
	if (selectedFOV === null) {
	    return;
	}

	const fov = selectedFOV;
	fov.fov.set_lineColor(fov.color);
	fov.fov.set_lineWidth(fov.linewidth);
	fov.fov.set_opacity(fov.opacity);

	selectedFOV = null;
    }
    
    // Hide previously-selected obsids.
    //
    // This was originally written when it was planned to have multiple
    // panes open, but now moving towards to only having a single pane
    // displayed at a time.
    //
    function unselectObsIds() {
	const host = getHost();
	if (host === null) {
	    return;
	}

	host.querySelectorAll('.statusPane')
	    .forEach(el => el.style.display = 'none');

	resetFOV();
    }

    // Display the "current" timeline; this is an experiment to see
    // how things work together and will need to be expanded.
    //
    function showTimeline(running) {

	const host = document.querySelector('#timeline');
	if (host === null) {
	    console.log("Internal error: unable to find #timeline");
	    return;
	}

	// TODO: need to clear out previous settings

	// current shouldn't be null, but makes it easier to repeat code.
	//
	const current = host.querySelector('#current');
	if (running.current !== null) {
	    current.setAttribute('data-obsid', running.current);

	    const acurrent = document.createElement('a');
	    acurrent.html = '#';
	    acurrent.innerText = running.current.toString();
	    acurrent.addEventListener('click', e => showObsId(running.current));
	    current.appendChild(acurrent);
	} else {
	    current.innerHTML = document.createTextNode('unknown');
	    current.removeAttribute('data-obsid');
	}

	const prev = host.querySelector('#previous');
	if (running.previous !== null) {
	    prev.setAttribute('data-obsid', running.previous);

	    const aprev = document.createElement('a');
	    aprev.html = '#';
	    aprev.innerText = running.previous.toString();
	    aprev.addEventListener('click', e => showObsId(running.previous));
	    prev.appendChild(aprev);
	} else {
	    prev.innerHTML = document.createTextNode('unknown');
	    prev.removeAttribute('data-obsid');
	}

	const next = host.querySelector('#next');
	if (running.next !== null) {
	    next.setAttribute('data-obsid', running.next);

	    const anext = document.createElement('a');
	    anext.html = '#';
	    anext.innerText = running.next.toString();
	    anext.addEventListener('click', e => showObsId(running.next));
	    next.appendChild(anext);

	} else {
	    next.innerHTML = document.createTextNode('unknown');
	    next.removeAttribute('data-obsid');
	}

	// Using grid - this has already been done by wwtReadyFunc
	// host.style.display = 'grid';
    }

    function showObsId(obsid) {

	unselectObsIds();
	selectFOV(obsid);  // ignore return value for now
		
	const idVal = 'obsid-' + obsid;

	const foundPane = document.querySelector('#' + idVal);
	if (foundPane !== null) {

	    const ra = foundPane.getAttribute('data-ra');
	    const dec = foundPane.getAttribute('data-dec');

	    if ((ra !== null) && (dec !== null)) {
		wwt.gotoRaDecZoom(ra, dec, defaultFieldSize, false);
	    }

	    // TODO: need to pan to the observation if it is an
	    //       engineering observation (ie has no FOV).
	    //
	    foundPane.style.display = 'block';
	    return;
	}
	
        $.ajax({
	    url: "/api/page/" + obsid,
	    dataType: "json"
        }).done(function (rsp) {

	    const host = getHost();
	    if (host === null) {
		return;
	    }

	    const pane = document.createElement('div');
	    pane.setAttribute('class', 'statusPane');
	    pane.setAttribute('id', idVal);

	    // Note: add ra and dec to the pane, since we can then
	    //       extract it from the pane rather than searching
	    //       around from it (which can be awkward for engineering
	    //       observations).
	    //
	    pane.setAttribute('data-ra', rsp.ra);
	    pane.setAttribute('data-dec', rsp.dec);
	    
	    pane.draggable = true;
	    pane.addEventListener('dragstart',
				  event => draggable.startDrag(event));

	    const controlElements = document.createElement('div');
	    controlElements.setAttribute('class', 'controlElements');

	    const title = document.createElement('span');
	    title.setAttribute('class', 'title');
	    title.innerText = "Observation " + obsid;
	    controlElements.appendChild(title);
	    
	    pane.appendChild(controlElements);

	    const main = document.createElement('div');
	    main.setAttribute('class', 'main');

	    pane.appendChild(main);
	    
	    controlElements.appendChild(addCloseButton(pane));
	    controlElements.appendChild(addHideShowButton(main));

            if (rsp.status === 'success') {

		// We create the tabs and then decide whether to
		// add it to the main element (we don't for engineering
		// observations as only have a details tab).
		//
		const tabs = document.createElement('ul');
		tabs.setAttribute('class', 'sections');

		const content = document.createElement('div');
		content.setAttribute('class', 'content');
		
		// Set up the the tabs and their content.
		//
		var statusLi = null;
		var ntabs = 0;
		
		[['observation', 'Status'],
		 ['related', 'Related'],
		 ['details', 'Details'],
		 ['proposal', 'Proposal']].forEach((x) => {
		     const field = x[0];
		     const label = x[1];
		     if (field in rsp) {
			 const vals = make_display_elements(pane, rsp, field, label);
			 tabs.appendChild(vals.li);
			 content.appendChild(vals.content);

			 if (field === 'observation') { statusLi = vals.a; }
			 ntabs += 1;
		     }
		 });

		if (ntabs > 1) {
		    main.appendChild(tabs);
		}
		
		main.appendChild(content);
		
                // add click handlers to the obsid links
                //
		content.querySelectorAll('a.obsidlink').forEach((el) => {
		    const obsid = parseInt(el.getAttribute('data-obsid'));
		    if (isNaN(obsid)) {
			console.log("Internal error: data-obsid=[" +
				    el.getAttribute('data-obsid') +
				    "] not an int!");
			return;
		    }
		    el.addEventListener('click', e => showObsId(obsid));
		});

		wwt.gotoRaDecZoom(rsp.ra, rsp.dec, defaultFieldSize, false);

		// Set up the display now everything has been added.
		//
		if (statusLi !== null) {
		    statusLi.click();
		}

            } else {

		const err = document.createElement('div');
		err.setAttribute('class', 'error');
		err.innerHTML = rsp.error;

		main.appendChild(err);
            }

	    host.appendChild(pane);
	    
        }).fail(function(xhr, status, e) {
	    console.log("QUERY FAILED: " + status);
            serverGoneByBy();
        });
    }

    function showCurrent() {
        $.ajax({
            url: "/api/current",
            dataType: "json"
        }).done(function(rsp) {
            if (rsp[0] === 'Success') {
                showObsId(rsp[1]['current']);
		showTimeline(rsp[1]);
		
            } else {
		console.log("WARNING: /api/current returned " + rsp[0]);
		console.log(rsp);

		const host = getHost();
		if (host === null) {
		    alert("I can not find out what Chandra is doing\n" +
			  "and strange things are afoot at the Circle K\n" +
			  "- err I mean the web page - and I do not\n" +
			  "know what to do!");
		    return;
		}

		const div = document.createElement('div');
		div.setAttribute('class', 'no-current-response');

		div.innerHTML = "<p>There was a problem and I was unable " +
		    "to find out what Chandra is doing now.</p>";

		host.appendChild(div);
            }
        }).fail(function(xhr, status, e) {
	    console.log("QUERY FAILED: " + status);
            serverGoneByBy();
        });
    }

    // This assumes that the FOVs have been loaded.
    //
    function wwtReadyFunc() {

	// Set up buttons
	//
	const ichoice = document.querySelector('#imagechoice');
	if (ichoice !== null) {
	    ichoice.addEventListener('change',
				     e => changeBackground(e.target.value));
	}

	// NED/SIMBAD seatch buttons
	//
	const search_ned = document.querySelector('#search-ned');
	if (search_ned !== null) {
	    search_ned.addEventListener('click', () => searchNear('ned'));
	}

	const search_sim = document.querySelector('#search-simbad');
	if (search_sim !== null) {
	    search_sim.addEventListener('click', () => searchNear('simbad'));
	}

        // TODO: should this check that the name is not blank/empty?
	//
	const tfind = document.querySelector("#targetFind");
	if (tfind !== null) {
	    tfind
		.addEventListener("click", () => { findTargetName(); });
	}

        /* Allow the user to start the search by hitting enter in the
         * search text box. The search button is only active when
         * there is text.
         */
	const tname = document.querySelector("#targetName");
	if (tname !== null) {
	    tname
		.addEventListener("keyup",
				  (e) => {
                                      const val = tname.value.trim();
                                      const button = document.querySelector("#targetFind");

                                      // Ensure the submit button is only enabled when there is
                                      // any text.
                                      if (val === "") {
					  button.disabled = true;
					  return;
				      } else if (button.disabled) {
					  button.disabled = false;
				      };

                                      if (e.keyCode !== 13) { return; }
                                      button.click();
				  });
	}

	// Set up the main display
	//
	createFOVs();
	showCurrent();
	
	// Find the nearest FOV to the user's click.
	//
	wwt.add_clicked((obj, eventArgs) => {
	    let ra = eventArgs.get_RA();
	    if (ra < 0) { ra += 360.0; }
	    const dec = eventArgs.get_dec();
	    identifyNearestObsId(ra, dec);
	});

	// Set up the location field
	showLocation();

	// Display the control elements. Should probably be in a
	// single structure to make this easier (and to ensure things
	// don't overlap).
	//
	['control', 'location'].forEach((n) => {
	    document.querySelector('#' + n).style.display = 'block';
	});
	document.querySelector('#timeline').style.display = 'grid';
    }


    // Hack up an approximate field of view; this does
    // not account for any offset between the given
    // position and the aimpoint of the instrument.
    //
    // TODO: handle overlap at ra=0/360 boundary?
    //
    function shiftFOV(obsdata) {

        let ra = obsdata.ra;
        let dec = obsdata.dec;
        let roll = obsdata.roll;
        let instrument = obsdata.instrument;

        // assume center is at 0,0; units are in degrees,
        // no roll.
        let aw = 8.0/60.0; // width of ACIS chip in degrees
        let hiw = 30.0/60.0; // width of HRC-I chip in degrees
        let aw2 = aw/2;
        let hiw2 = hiw/2;
        let hsw = 99.0 / 60.0;
        let hsh = 6.0 / 60.0; // if use 3.4 then need to deal with offset
        let hsw2 = hsw / 2;
        let hsh2 = hsh / 2;
        let p0;
        if (instrument === "ACIS-S") {
            p0 = [[-3*aw,-aw2], [-3*aw,aw2], [3*aw,aw2], [3*aw,-aw2], [-3*aw,-aw2]];
        } else if (instrument === "HRC-I") {
            // Rather than rotate this by 45 degrees, add to the roll
            // angle.
            p0 = [[-hiw2,-hiw2], [-hiw2,hiw2], [hiw2,hiw2], [hiw2,-hiw2], [-hiw2,-hiw2]];
            roll += 45.0;
            
        } else if (instrument === "HRC-S") {
            p0 = [[-hsw2,-hsh2], [-hsw2,hsh2], [hsw2,hsh2], [hsw2,-hsh2], [-hsw2,-hsh2]];
        } else {
            // fall back to ACIS-I
            p0 = [[-aw,-aw], [-aw,aw], [aw,aw], [aw,-aw], [-aw,-aw]];
        }
        
        let p1 = [];

        // how is roll defined? clockwise? where is roll=0?
        // It seems that this is correct by comparing to the
        // DSS result
        let ang = roll * Math.PI / 180.0;
        let cosang = Math.cos(ang);
        let sinang = Math.sin(ang);
        let cosdec = Math.cos(dec * Math.PI / 180.0);

        for (const p of p0) {
            let x0 = p[0];
            let y0 = p[1];

            let dx = (x0 * cosang - y0 * sinang) / cosdec;
            let x1 = ra + dx;
            let y1 = dec + x0 * sinang + y0 * cosang;

            p1.push([x1, y1]);
        }

        return p1;
    }

    function makeFOV(obsdata) {
	const fov = wwt.createPolyLine(true);

	fov.set_id("ObsId " + obsdata.obsid);
	fov.set_label(obsdata.name);

	// set up for "not current" obsid
	//
	fov.set_lineColor(fovColor);
	fov.set_lineWidth(fovLineWidth);
	fov.set_opacity(fovOpacity);

        for (const p of shiftFOV(obsdata)) {
            fov.addPoint(p[0], p[1]);
        }

	return fov;	
    }

    // We need to
    //  a) convert the FOV data from the API into structures used
    //     here (that is, set up the obsdatas array)
    //  b) create FOV regions from the obsdatas array, which requires
    //     WWT to be set up.
    //
    var obsdatas = null;
    function parseFOVs(rsp) {
	if (rsp.length === 0) {
	    console.log("*** NO FOV data returned!");
	    return;
	}
	obsdatas = rsp;
    }
    
    var fovRegions = null;
    function createFOVs() {

	fovRegions = [];
	obsdatas.forEach((obsdata) => {
	    const fov = makeFOV(obsdata)
	    fovRegions.push(fov);
	    wwt.addAnnotation(fov);
	});
    }

    // Return the angular sepration between the two points
    // Based on https://en.wikipedia.org/wiki/Great-circle_distance
    // and not worrying about rounding errors
    //
    // Input arguments are in degrees, as is the
    // return value.
    //
    function separation(ra1, dec1, ra2, dec2) {
        var lat1 = dec1 * Math.PI / 180.0;
        var lat2 = dec2 * Math.PI / 180.0;
        var dlon = Math.abs(ra1 - ra2) * Math.PI / 180.0;
        
        var term = Math.sin(lat1) * Math.sin(lat2) +
            Math.cos(lat1) * Math.cos(lat2) * Math.cos(dlon);
        return Math.acos(term) * 180.0 / Math.PI;
    }

    // What is near the given location (in degrees).
    //
    function identifyNearestObsId(ra, dec) {
	// Search out to the approximate screen size
	const maxSep = wwt.get_fov();

	var seps = [];
	for (var i = 0; i < obsdatas.length; i++) {
	    const obsdata = obsdatas[i];
	    const sep = separation(ra, dec, obsdata.ra, obsdata.dec);
	    if (sep <= maxSep) {
		seps.push([sep, i]);
	    }
	}

	if (seps.length === 0) {
	    return;
	}
	
	// Sort on the separation (first item; could sort by name or obsid
	// as a secondary step but not sure it's worth it).
	//
	seps.sort(function (a, b) { if (a[0] < b[0]) { return -1; }
				    else if (a[0] > b[0]) { return 1; }
				    else { return 0; } });
    

	const idx = seps[0][1];
	showObsId(obsdatas[idx].obsid);
    }

    // Returns if a FOV could be found or not.
    //
    function selectFOV(obsid) {

	if (fovRegions === null) {
	    console.log("*** ALERT: asked to show fov for " + obsid + " but no data loaded!");
	    return false;
	}

	const idx = obsdatas.findIndex(obsdata => obsdata.obsid === obsid);
	if (idx < 0) {
	    // Assume that this is a non-science observation.
	    return false;
	}

	const fov = fovRegions[idx];

	// Store the original setting.
	//
	selectedFOV = {fov: fov,
		       color: fov.get_lineColor(),
		       linewidth: fov.get_lineWidth(),
		       opacity: fov.get_opacity()};
	
	fov.set_lineColor(fovSelectedColor);
	fov.set_lineWidth(fovSelectedLineWidth);
	fov.set_opacity(fovSelectedOpacity);

	// Want to ensure this annotation is "on top" so remove and add it,
	// which seems excessive but I do not know a better way with the
	// WWT API.
	//
	wwt.removeAnnotation(fov);
	wwt.addAnnotation(fov);

	return true;
    }
    
    /*
     * Does the browser support full-screen mode?
     * The logic for this is taken directly from
     * https://github.com/WorldWideTelescope/wwt-website/blob/master/WWTMVC5/Scripts/controls/WebControl.js
     *
     * Could hard-code the supported methods, but not worth it for now
     */
    function hasFullScreen() {
	var el = document.body;
	for (var name of ['requestFullScreen',
			  'mozRequestFullScreen',
			  'webkitRequestFullScreen',
			  'msRequestFullScreen']) {
	    if (name in el) { return true; }
	}
	return false;
    }

    function startFullScreen() {
	const el = document.body;
	for (var name of ['requestFullScreen',
			  'mozRequestFullScreen',
			  'webkitRequestFullScreen',
			  'msRequestFullScreen']) {
	    if (name in el) {
		document.getElementById('togglefullscreen').innerHTML =
		    'Normal screen';
		el[name]();
		return;
	    }
	}
	console.log("UNEXPECTED: failed to call startFullScreen");
    }

    function stopFullScreen() {
	const el = document;
	for (var name of ['cancelFullScreen',
			  'mozCancelFullScreen',
			  'webkitCancelFullScreen',
			  'msExitFullscreen']) {
	    if (name in el) {
		document.getElementById('togglefullscreen').innerHTML =
		    'Full screen';
		el[name]();
		return; }
        }
	alert("Eek! Unable to cancel full screen.\nTry hitting Escape.");
    }

    var fullscreen = false;
    var entering_fullscreen = false;
    function toggleFullScreen() {
        var fn;
        if (fullscreen) {
	    fn = stopFullScreen;
        } else {
	    fn = startFullScreen;
	    entering_fullscreen = true;
        }

	// Change fullscreen setting before calling the routine
	// since we use it in resize() to indicate the expected
	// mode.
	//
        fullscreen = !fullscreen;
	fn();
    }


    /*
     * User's window has been resized (or setting up the initial size).
     */
    function resize() {

        const width = window.innerWidth;
        const height = window.innerHeight;

	const div = document.getElementById("WWTCanvas");

        const wstr = width.toString() + "px";
        if (div.style.width !== wstr ) {
            div.style.width = wstr;
        }

        const hstr = height.toString() + "px";
        if (div.style.height !== hstr) {
            div.style.height = hstr;
        }

	/***
	// Try to work out if the user has exited full-screen mode
	//
	const el = document.getElementById('togglefullscreen');
	if (fullscreen) {

	    if (entering_fullscreen) {
		entering_fullscreen = false;
	    } else {
		el.innerHTML = 'Full screen';
		fullscreen = false;
	    }
	} else if (el.innerHTML !== 'Full screen') {
	    el.innerHTML = 'Full screen';
	}
	***/
	
    }


    // Is this a positive (including 0) float?
    // No exponential notation supported here.
    //
    const posFloatRegex = /^(\+)?([0-9]+)?(\.[0-9]+)?$/;
    function posFloat(s) {
	return posFloatRegex.test(s);
    }

    // How about supporting negative numbers
    const anyFloatRegex = /^(\-|\+)?([0-9]+)?(\.[0-9]+)?$/;
    function anyFloat(s) {
	return anyFloatRegex.test(s);
    }

    // Convert a number to a RA (in decimal degrees). Supported input
    // are:
    //    decimal degrees
    //    hours(int) [ :h] minutes(float) [:m]
    //    hours(int) [ :h] minutes(float) [:m] seconds(float) [:s]
    // where white space is allowed between the tokens
    //
    // There is no requirement that the labels match - e.g. at present
    //   23h 45:23
    //   23h 45.23
    //   23 45:23
    // are all supported.
    //
    // No guarantee on error checking, or nice, clean code.
    //
    const hourRegex = /^(\d\d?)\s*[:hH]?\s*(.*)$/;
    const minRegex = /^(\d\d?(\.[\d]*)?)\s*[:mM]?\s*(.*)$/;
    const secRegex = /^(\d\d?(\.[\d]*)?)\s*[sS]$/;

    function strToRA(str) {
	var sval = str.trim();

	if (posFloat(sval)) {
	    var ra = parseFloat(sval);
	    if ((ra < 0) | (ra > 360.0)) {
		return null;
	    }
	    return ra;
	}

	// Separators can be ' ', ':', 'h', 'm', 's' although
	// some of these are positional (h/m/s)
	//
	const hr = hourRegex.exec(sval);
	if (hr === null) {
	    return null;
	}

	// Could use parseInt here
	const h = parseFloat(hr[1]);
	if ((h < 0.0) || (h > 24.0)) {
	    return null;
	}

	// now look for minutes
	sval = hr[2];

	if (sval.trim() === "") {
	    return 15.0 * h;
	}

	if (posFloat(sval)) {
	    var m = parseFloat(sval);
	    if ((m < 0.0) || (m > 60.0)) {
		return null;
	    }
	    return 15.0 * (h + (m / 60.0));
	}

	const mr = minRegex.exec(sval);
	if (mr === null) {
	    return null;
	}

	m = parseFloat(mr[1]);
	if ((m < 0.0) || (m > 60.0)) {
	    return null;
	}

	// now look for seconds
	sval = mr[3];

	if (sval.trim() === "") {
	    return 15.0 * (h + (m / 60.0));
	}

	if (posFloat(sval)) {
	    var s = parseFloat(sval);
	    if ((s < 0.0) || (s > 60.0)) {
		return null;
	    }
	    return 15.0 * (h + (m + (s / 60.0)) / 60.0);
	}

	const sr = secRegex.exec(sval);
	if (sr === null) {
	    return null;
	}

	s = parseFloat(sr[1]);
	if ((s < 0.0) || (s > 60.0)) {
	    return null;
	}

        return 15.0 * (h + (m + (s / 60.0)) / 60.0);
    }

    // Convert a number to a Dec (in decimal degrees). Supported input
    // are:
    //    decimal degrees
    //    (+-)degrees(int) [ :d] minutes(float) [:m']
    //    (+-)degrees(int) [ :d] minutes(float) [:m'] arcseconds(float) [:s"]
    // where white space is allowed between the tokens
    //
    // There is no requirement that the labels match - e.g. at present
    //   23d 45:23
    //   23d 45.23
    //   23 45:23
    // are all supported.
    //
    // No guarantee on error checking.
    //
    const degRegex = /^(\d\d?)\s*[:dD]?\s*(.*)$/;
    const dminRegex = /^(\d\d?(\.[\d]*)?)\s*[:mM']?\s*(.*)$/;
    const dsecRegex = /^(\d\d?(\.[\d]*)?)\s*[sS"]$/;

    function strToDec(str) {
	var sval = str.trim();

	if (anyFloat(sval)) {
	    var dec = parseFloat(sval);
	    if ((dec < -90) || (dec > 90.0)) {
		return null;
	    }
	    return dec;
	}

	var sign = 1.0;
	if (sval.startsWith('-')) {
	    sign = -1.0;
	    sval = sval.slice(1).trim();
	} else if (sval.startsWith('+')) {
	    sval = sval.slice(1).trim();
	}

	// Separators can be ' ', ':', 'd', 'm', 's', ' and "" although
	// some of these are positional (e.g. d/m/s)
	//
	const dr = degRegex.exec(sval);
	if (dr === null) {
	    return null;
	}

	// Note that for a negative value to appear
	// here there must be something like '--' or '+-', which is
	// invalid, hence the positive check
	const d = parseFloat(dr[1]);
	if ((d < 0.0) || (d > 90.0)) {
	    return null;
	}

	// now look for minutes
	sval = dr[2];

	if (sval.trim() === "") {
	    return sign * d;
	}

	if (posFloat(sval)) {
	    var m = parseFloat(sval);
	    if ((m < 0.0) || (m > 60.0)) {
		return null;
	    }
	    return sign * (d + (m / 60.0));
	}

	const mr = dminRegex.exec(sval);
	if (mr === null) {
	    return null;
	}

	m = parseFloat(mr[1]);
	if ((m < 0.0) || (m > 60.0)) {
	    return null;
	}

	// now look for seconds
	sval = mr[3];

	if (sval.trim() === "") {
	    return sign * (d + (m / 60.0));
	}

	if (posFloat(sval)) {
	    var s = parseFloat(sval);
	    if ((s < 0.0) || (s > 60.0)) {
		return null;
	    }
	    return sign * (d + (m + (s / 60.0)) / 60.0);
	}

	const sr = dsecRegex.exec(sval);
	if (sr === null) {
	    return null;
	}

	s = parseFloat(sr[1]);
	if ((s < 0.0) || (s > 60.0)) {
	    return null;
	}

        return sign * (d + (m + (s / 60.0)) / 60.0);
    }


    // click handler for targetFind.
    // - get user-selected name
    // - is it a position; if so jump to it
    // - otherwise send it to the lookUP service
    //
    function findTargetName() {
        var target = document.querySelector("#targetName").value;
	if (target.trim() === "") {
	    // this should not happen, but just in case
	    console.log("Unexpected targetName=[" + target + "]");
	    return;
	}

	// For now use a single comma as a simple indicator that we have
	// a location, and not a target name.
	//
	const toks = target.split(",");

	if (toks.length === 2) {
	    const ra = strToRA(toks[0]);
	    if (ra !== null) {
		const dec = strToDec(toks[1]);
		if (dec !== null) {

		    const zoom = wwt.get_fov();
		    wwt.gotoRaDecZoom(ra, dec, zoom);
		    return;
		}
	    }
	}

	// Got this far, assume the input was a name to be resolved.
	//
        lookup.lookupName(target);
    }


    function initialize() {

	resize();
	
	// Load in the data
	//
	$.ajax({url: '/api/allfov', dataType: 'json'})
	    .done(parseFOVs)
	    .then(() => {
		wwt = wwtlib.WWTControl.initControl("WWTCanvas");
		wwt.loadImageCollection("/chandra.wtml");
		wwt.add_ready(wwtReadyFunc);
		wwt.endInit();

		/*
		 * Stop scroll events from being passed up to the browser,
		 * since it is a less-than-ideal UI. Is this the best place
		 * for it; perhaps within wwtReadyFunc?
		 */
		const canvas = document.getElementById("WWTCanvas");
		canvas.addEventListener("mousewheel", e => e.preventDefault());
		canvas.addEventListener("DOMMouseScroll", e => e.preventDefault());

		// The following is taken from ADS all-sky-survey, not entirely
		// sure if needed here.
		//
		canvas.addEventListener('mouseout',
					event => wwtlib.WWTControl.singleton.onMouseUp(event));

		// Set up the drag handlers
		//
		const host = getHost();
		host.addEventListener('dragover',
				      event => event.preventDefault());
		host.addEventListener('drop',
				      event => draggable.stopDrag(event));

	    })
	    .fail((xhr, status, e) => {
		// TODO: better error handling
		console.log("FAILED /api/allfov call");
		alert("There was a problem setting up the page.");
	    });
	
    }

    /*
     * What data is the WWT showing? It would be nice to allow foreground and
     * background images, with an opacity setting, but this may be a confusing
     * UI so just stick with a single image for the time being.
     *
     * The wtml "dictionary" mapping from short to long names is based on an
     * analysis of a WTML file created by WWT on Windows. Not all images
     * are planned to be used, but left in for now.
     */
    var wtml = {'wmap': 'WMAP ILC 5-Year Cosmic Microwave Background',
                'dust': 'SFD Dust Map (Infrared)',
                '2mass-cat': '2MASS: Catalog (Synthetic, Near Infrared)',
                '2mass-image': '2Mass: Imagery (Infrared)',
                'dss': 'Digitized Sky Survey (Color)',
                'vlss': 'VLSS: VLA Low-frequency Sky Survey (Radio)',
                'planck-cmb': 'Planck CMB',
                'planck-dust-gas': 'Planck Dust & Gas',
                'iris': 'IRIS: Improved Reprocessing of IRAS Survey (Infrared)',
                'wise': 'WISE All Sky (Infrared)',
                'halpha': 'Hydrogen Alpha Full Sky Map',
                'sdss': 'SDSS: Sloan Digital Sky Survey (Optical)',
                'tycho': 'Tycho (Synthetic, Optical)',
                'usnob1': 'USNOB: US Naval Observatory B 1.0 (Synthetic, Optical)',
                'galex4-nuv': 'GALEX 4 Near-UV',
                'galex4-fuv': 'GALEX 4 Far-UV',
                'galex': 'GALEX (Ultraviolet)',
                'rass': 'RASS: ROSAT All Sky Survey (X-ray)',
                'fermi3': 'Fermi Year Three (Gamma)'
               };

    // Change the "background" image being displayed by WWT.
    //
    function changeBackground(background, opacity=100.0) {
	if (background === "dss") {
	    wwt.setForegroundOpacity(0.0);
	    return;
	}
	
        const fullName = wtml[background];
        if (typeof fullName === "undefined") {
            console.log("Unknown name: " + background);
            return;
        }
        wwt.setForegroundImageByName(fullName);
        wwt.setForegroundOpacity(opacity);
    }

    // Search near the current location in a particular (external)
    // database.
    //
    // For now use a 2 arcminute radius.
    //
    // The UI isn't ideal here; have a "select database" style
    // option as a label/value, so do nothing if this is selected.
    //
    function searchNear(database, rmax=2) {
	if (database === 'unselected') {
	    return;
	}

	const ra = 15.0 * wwt.getRA();
	const dec = wwt.getDec();

	var url = null;
	if (database === 'ned') {

	    const raElems = raToTokens(ra);
	    const decElems = decToTokens(dec);

	    const raStr = i2(raElems.hours) + "h" +
		  i2(raElems.minutes) + "m" +
		  f2(raElems.seconds, 2) + "s";

	    var decSign;
	    if (dec < 0.0) { decSign = "-"; } else { decSign = "+"; }

	    const decStr = decSign +
		  i2(decElems.degrees) + "d" +
		  i2(decElems.minutes) + "'" +
		  f2(decElems.seconds, 1) + '"';

	    /*** I can not get the "new" version to work, in that it
                 seems to not submit the actual search

	    url = 'https://ned.ipac.caltech.edu/conesearch?search_type=Near%20Position%20Search&iau_style=liberal&coordinates=' +
		raStr + '%20' + decStr +
		'&radius=1&in_csys=Equatorial&in_equinox=J2000.0&in_csys_IAU=Equatorial&in_equinox_IAU=B1950&z_constraint=Unconstrained&z_unit=z&ot_include=ANY&nmp_op=ANY&hconst=67.8&omegam=0.308&omegav=0.692&wmap=4&corr_z=1&out_csys=Same%20as%20Input&out_equinox=Same%20as%20Input&obj_sort=Distance%20to%20search%20center';
	    ***/

	    url = 'https://ned.ipac.caltech.edu/cgi-bin/objsearch?search_type=Near+Position+Search&in_csys=Equatorial&in_equinox=J2000.0' +
		'&lon=' + raStr +
		'&lat=' + decStr +
		'&radius=' + rmax.toString() +
		'&hconst=73&omegam=0.27&omegav=0.73&corr_z=1&z_constraint=Unconstrained&z_value1=&z_value2=&z_unit=z&ot_include=ANY&nmp_op=ANY&out_csys=Equatorial&out_equinox=J2000.0&obj_sort=Distance+to+search+center&of=pre_text&zv_breaker=30000.0&list_limit=5&img_stamp=YES';

	} else if (database === 'simbad') {
	    url = "http://simbad.harvard.edu/simbad/sim-coo?Coord=" +
		ra.toString() + "+" +
		dec.toString() + "&CooFrame=FK5&CooEpoch=2000" +
		"&CooEqui=2000&CooDefinedFrames=none" +
		"&Radius=" + rmax.toString() +
		"&Radius.unit=arcmin" +
		"&submit=submit+query&CoordList=";
	} else {
	    console.log("INTERNAL ERROR: unknown search database '" +
			database + "'");
	}

	if (url !== null) {
	    window.open(url);
	}

	// Reset the search option. I don't think this triggers
	// this routine, but if it does it should short-cut immediately.
	//
	// Assume we want the first option
	const option = document.querySelector('#searchengine option');
	if (option === null) {
	    console.log("INTERNAL ERROR #searchengine not found");
	    return;
	}

	option.selected = true;
    }

    // Convert RA (in degrees) into hour, minutes, seconds. The
    // return value is an object.
    //
    function raToTokens(ra) {
        ra /= 15.0;

        var rah = Math.floor(ra);
        var delta = 60.0 * (ra - rah);
        var ram = Math.floor(delta);
        var ras = 60.0 * (delta - ram);
        ras = Math.floor(ras * 100.0 + 0.5) / 100.0;

	return {hours: rah, minutes: ram, seconds: ras};
    }

    // Convert Dec (in degrees) into sign, degree, minutes, seconds. The
    // return value is an object.
    //
    function decToTokens(dec) {
        const is_neg = dec < 0.0;
        var sign;
        if (is_neg) { sign = "-"; } else { sign = "+"; }

        dec = Math.abs(dec);
        const decd = Math.floor(dec);
        const delta = 60.0 * (dec - decd);
        const decm = Math.floor(delta);
        var decs = 60.0 * (delta - decm);
        decs = Math.floor(decs * 10.0 + 0.5) / 10.0;

	return {sign: sign, degrees: decd, minutes: decm, seconds: decs};
    }

    // integer to "xx" format string, 0-padded to the left.
    //
    function i2(x) {
	return x.toString().padStart(2, '0');
    }

    // float to "xx.y" format, where the number of decimal places is ndp
    function f2(x, ndp) {
	return x.toFixed(ndp).padStart(3 + ndp, '0')
    }
    
    // Convert RA (degrees) to HTML, using a fixed format (ie width)
    function raToHTML(ra) {
	const toks = raToTokens(ra);
	return i2(toks.hours) + "<sup>h</sup> " +
	    i2(toks.minutes) + "<sup>m</sup> " +
	    f2(toks.seconds, 2) + "<sup>s</sup>";
    }

    // Convert Dec (degrees) to HTML
    function decToHTML(dec) {
	const toks = decToTokens(dec);
	return toks.sign +
	    i2(toks.degrees) + "° " +
	    i2(toks.minutes) + "' " +
	    f2(toks.seconds, 1) + '"';
    }

    // Update with the current location; as there doesn't seem to be
    // a way for WWT to report that it is moving its "aim point" then
    // we just check every n seconds.
    //
    // This is an experiment.

    var lastLoc = "";
	
    function showLocation() {

	// Should really cache this
	const location = document.querySelector('#location-value');
	
	const ra = 15.0 * wwt.getRA();
	const dec = wwt.getDec();

	const newLoc = raToHTML(ra) + " " + decToHTML(dec);
	if (lastLoc !== newLoc) {
	    location.innerHTML = newLoc;
	    lastLoc = newLoc;
	}

	// Set the new timer
	window.setTimeout(showLocation, 1000);
    }
    
    return {initialize: initialize,
	    resize: resize,

	    // make it easier to debug/explore things
	    showObsId: showObsId,
	    changeBackground: changeBackground,

	    // This is currently needed by the lookup module, which is
	    // not a great design.
	    //
	    getWWT: () => { return wwt; },
	    getHost: getHost,
	   };

})();

// end