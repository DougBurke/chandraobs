"use strict";

// Experiment with a WWT-based main display
//
// TODO:
//   there should probably only be one spinner "active" at a time, but
//   if they are located at the same location it probably isn't a problem
//   (unless the background is transparent or the animation "jumps" when
//   adding the new one on top of the old one).
//
//   Use a consistent naming scheme, as have FOV for the WWT FoV and
//   for the polygon representing an observation.
//
//   Use HTML template rather than creating HTML structure in JS
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
	const host = document.getElementById('WorldWideTelescopeControlHost');
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

    // Sent the div for the whole pane.
    function addCloseButton(parent) {
	const el = document.createElement('span');
	el.setAttribute('class', 'closable');
	el.addEventListener('click', () => {
	    parent.style.display = 'none';
	    resetFOV();
	});
	return el;
    }

    // Sent the div for the main contents and the title sections
    //
    function addHideShowButton(parent, titleBar) {
	const el = document.createElement('span');
	el.classList.add('switchable');
	el.classList.add('hideable');
	el.addEventListener('click', () => {
	    if (el.classList.contains('hideable')) {
		parent.style.display = 'none';
		el.classList.remove('hideable');
		el.classList.add('showable');
		titleBar.classList.remove('controlElementsShown');
	    } else {
		parent.style.display = 'block';
		el.classList.remove('showable');
		el.classList.add('hideable');
		titleBar.classList.add('controlElementsShown');
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

    // Based on
    // https://stackoverflow.com/questions/3955229/remove-all-child-elements-of-a-dom-node-in-javascript
    //
    function removeChildren(node) {
	while (node.firstChild) {
	    node.removeChild(node.firstChild);
	}
    }

    function changeText(host, selector, text) {
	const elem = host.querySelector(selector);
	if (elem === null) {
	    console.log("Internal error: no match for '" + selector + "'");
	    return;
	}

	removeChildren(elem);  // is this needed?
	elem.innerText = text;

    }

    // Let the user know what the current obsid is
    //
    function showCurrentTimeLine(obsdata) {
	const node = document.getElementById('timeline-selected-obs');
	if (node === null) {
	    console.log("Internal error: unable to find #timeline-selected-obs");
	    return;
	}

	removeChildren(node);

	if (obsdata !== null) {
	    node.setAttribute('data-obsid', obsdata.obsid);

	    const link = document.createElement('a');
	    link.html = '#';
	    link.innerText = "" + obsdata.target;
	    link.addEventListener('click',
				  e => showObsId(obsdata.obsid));
	    node.appendChild(link);

	} else {
	    node.appendChild(document.createTextNode('unknown'));
	    node.removeAttribute('data-obsid');
	}
    }

    function textNode(txt) {
	return document.createTextNode(txt);
    }

    // Wrap contents up with a span
    function addSpan(cts) {
	const span = document.createElement('span');
	span.appendChild(cts);
	return span;
    }
    
    // Create a link with the given label that jumps to the given obsid
    function obsidLink(obsid, label) {
	const a = document.createElement('a');
	a.setAttribute('href', '#');
	a.setAttribute('data-obsid', obsid);
	
	a.textContent = label;
	a.addEventListener('click', () => showObsId(obsid));
	return a;
    }

    // Make a div a "pane".
    //
    function setupPane(pane, title) {

	const controlElements = document.createElement('div');
	controlElements.classList.add('controlElements');
	controlElements.classList.add('controlElementsShown');

	const titleEl = document.createElement('span');
	titleEl.setAttribute('class', 'title');
	titleEl.innerText = title;
	controlElements.appendChild(titleEl);

	pane.appendChild(controlElements);

	const main = document.createElement('div');
	main.setAttribute('class', 'main');

	// Close elements require main so need to do it slightly
	// out of order
	controlElements.appendChild(addCloseButton(pane));
	controlElements.appendChild(addHideShowButton(main, controlElements));

	pane.appendChild(main);
	return main;
    }
    
    // Create a "status pane" showing the given observation details
    // from the /api/page/<obsid> query.
    //
    // This also tells the WWT to jump to the location (on success)
    //
    function makeStatusPane(obsid, idVal, rsp) {

	const pane = document.createElement('div');
	pane.setAttribute('class', 'statusPane');
	pane.setAttribute('id', idVal);

	pane.draggable = true;
	pane.addEventListener('dragstart', draggable.startDrag);

	const main = setupPane(pane, "Observation " + obsid);

        if (rsp.status === 'success') {

	    // Note: add ra and dec to the pane, since we can then
	    //       extract it from the pane rather than searching
	    //       around from it (which can be awkward for engineering
	    //       observations).
	    //
	    pane.setAttribute('data-ra', rsp.ra);
	    pane.setAttribute('data-dec', rsp.dec);

	    // Set up links to the preceeding and next observation.
	    //
	    const timeline = document.createElement('div');
	    timeline.setAttribute('class', 'timeline');

	    const previous = document.createElement('span');
	    previous.setAttribute('class', 'previousobs');

	    const next = document.createElement('span');
	    next.setAttribute('class', 'nextobs');

	    main.appendChild(timeline);
	    timeline.appendChild(previous);
	    timeline.appendChild(next);

	    if (rsp.previous !== null) {
		previous.appendChild(obsidLink(rsp.previous.obsid,
					       rsp.previous.target));
		previous.appendChild(textNode(" \xAB"));
	    }

	    if (rsp.next !== null) {
		next.appendChild(textNode("\xBB "));
		next.appendChild(obsidLink(rsp.next.obsid,
					   rsp.next.target));
	    }

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

	    [['overview', 'Status'],
	     ['related', 'Related'],
	     ['details', 'Details'],
	     ['proposal', 'Proposal']].forEach((x) => {
		 const field = x[0];
		 const label = x[1];
		 if (field in rsp) {
		     const vals = make_display_elements(pane, rsp, field, label);
		     tabs.appendChild(vals.li);
		     content.appendChild(vals.content);

		     if (field === 'overview') { statusLi = vals.a; }
		     ntabs += 1;
		 }
	     });

	    if (ntabs > 1) {
		main.appendChild(tabs);

		// what div do we want to change?
		content.classList.add('scienceobs');
	    }

	    main.appendChild(content);

            // add click handlers to the obsid links
            //
	    content.querySelectorAll('a.obsidlink').forEach((el) => {
		const obsidLink = parseInt(el.getAttribute('data-obsid'));
		if (isNaN(obsid)) {
		    console.log("Internal error: data-obsid=[" +
				el.getAttribute('data-obsid') +
				"] not an int!");
		    return;
		}
		el.addEventListener('click', e => showObsId(obsidLink));
	    });

	    // Now for the related timeline link (should only be one)
	    //
	    content.querySelectorAll('.relatedlink').forEach((el) => {
		el.addEventListener('click', e => showRelated(obsid));
	    });

	    wwt.gotoRaDecZoom(rsp.ra, rsp.dec, defaultFieldSize, moveFlag);

	    // Set up the display now everything has been added.
	    //
	    if (statusLi !== null) {
		statusLi.click();
	    }

        } else {

	    const err = document.createElement('div');
	    err.setAttribute('class', 'error');
	    if (typeof rsp.error !== 'undefined') {
		err.innerHTML = rsp.error;
	    } else {
		err.innerHTML = "<p>An unknown error occurred.</p>";
	    }

	    main.appendChild(err);
        }

	return pane;
    }

    // Note: if the obsid is already selected (the pane already
    //       exists) then we just jump to the source.
    //
    //       Perhaps we should always recreate (or at least
    //       update the main text).
    //
    function showObsId(obsid) {

	const host = getHost();
	if (host === null) {
	    // if this has happened then lots of things are wrong
	    return;
	}

	unselectObsIds();
	selectFOV(obsid);  // ignore return value for now

	// We want to delete any related timeline that is displayed.
	// This is not ideal. Shouldn't we make sure any element
	// that is displaying the timeline has been closed down?
	//
	{
	    const pane = document.getElementById("vl-related");
	    if (pane !== null) {
		pane.style.display = "none";

		const main = pane.querySelector(".main");
		if (main !== null) {
		    main.querySelectorAll(".timeline").forEach((el) => {
			main.removeChild(el);
		    });
		}
	    }

	    const tl = document.getElementById('view-related-selector');
	    if (tl !== null && tl.innerText === hideRelatedText) {
		tl.innerText = viewRelatedText;
	    }

	    // Try to restore memory/resources
	    relatedResults.forEach((result) => result.finalize());
	    relatedResults = [];
	}

	const idVal = 'obsid-' + obsid;

	const foundPane = document.getElementById(idVal);
	if (foundPane !== null) {

	    const ra = foundPane.getAttribute('data-ra');
	    const dec = foundPane.getAttribute('data-dec');

	    // TODO: should this keep the current field size?
	    if ((ra !== null) && (dec !== null)) {
		wwt.gotoRaDecZoom(ra, dec, defaultFieldSize, moveFlag);
	    }

	    // This shouldn't be necessary, but leave in for now.
	    foundPane.style.display = 'block';
	    return;
	}

	const spin = spinner.createSpinner();
	host.appendChild(spin);
	
        $.ajax({
	    url: "/api/page/" + obsid,
	    cache: false,
	    dataType: "json"
        }).done(function (rsp) {
	    host.removeChild(spin);

	    const pane = makeStatusPane(obsid, idVal, rsp);
	    host.appendChild(pane);

        }).fail(function(xhr, status, e) {
	    host.removeChild(spin);

	    console.log("QUERY FAILED: " + status);
            serverGoneByBy();
        });
    }

    function showCurrent() {

	const host = getHost();
	if (host === null) {
	    // no way to sensibly handle this
	    return;
	}

	const spin = spinner.createSpinner();
	host.appendChild(spin);

        $.ajax({
            url: "/api/current",
            cache: false,
            dataType: "json"
        }).done(function(rsp) {

	    host.removeChild(spin);

            if (rsp[0] === 'Success') {
		showObsId(rsp[1].current.obsid);
		showCurrentTimeLine(rsp[1].current);

            } else {
		console.log("WARNING: /api/current returned " + rsp[0]);
		console.log(rsp);

		const div = document.createElement('div');
		div.setAttribute('class', 'no-current-response');

		div.innerHTML = "<p>There was a problem and I was unable " +
		    "to find out what Chandra is doing now.</p>";

		host.appendChild(div);
            }
        }).fail(function(xhr, status, e) {
	    host.removeChild(spin);

	    console.log("QUERY FAILED: " + status);
            serverGoneByBy();
        });
    }

    // This assumes that the FOVs have been loaded.
    //
    function wwtReadyFunc() {

	// Start with the DSS (keep as background).
	//
	wwt.setBackgroundImageByName(wtml['dss']);

	// Change background
	//
	const ichoice = document.getElementById('imagechoice');
	if (ichoice !== null) {
	    ichoice.addEventListener('change',
				     e => changeBackground(e.target.value));
	}

	// Do we have to support going fullscreen?
	if (hasFullScreen()) {
	    const fscreen = document.getElementById('control-fullscreen');
	    if (fscreen !== null) {
		const toggle = document.getElementById('togglefullscreen');
		if (toggle !== null) {
		    fscreen.style.display = 'list-item';
		    toggle.addEventListener('click',
					    () => toggleFullScreen(toggle));
		}
	    }
	}

	const tl = document.getElementById('view-timeline-selector');
	if (tl !== null) {
	    tl.addEventListener('click', () => showTimeline());
	}

	// NED/SIMBAD search buttons
	//
	const search_ned = document.getElementById('search-ned');
	if (search_ned !== null) {
	    search_ned.addEventListener('click', () => searchNear('ned'));
	}

	const search_sim = document.getElementById('search-simbad');
	if (search_sim !== null) {
	    search_sim.addEventListener('click', () => searchNear('simbad'));
	}

        // TODO: should this check that the name is not blank/empty?
	//
	const tfind = document.getElementById("targetFind");
	if (tfind !== null) {
	    tfind
	    .addEventListener("click", () => { findTargetName(); });
	}

        /* Allow the user to start the search by hitting enter in the
         * search text box. The search button is only active when
         * there is text.
         */
	const tname = document.getElementById("targetName");
	if (tname !== null) {
	    tname
		.addEventListener("keyup",
				  (e) => {
                                      const val = tname.value.trim();
                                      const button = document.getElementById("targetFind");

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

        // Set up the current date; reset every 10 seconds
        showCurrentDate();
        setInterval(showCurrentDate, 10000);

	// Set up the location field; reset every 2 seconds
        setInterval(showLocation, 2000);

	// Display the control elements. Should probably be in a
	// single structure to make this easier (and to ensure things
	// don't overlap).
	//
	['control'].forEach((n) => {
	    document.getElementById(n).style.display = 'block';
	});

	// Make the titlebar visible (hopefully this is after the
	// time/observation details are shown; we could make this
	// a certainty, ish).
	//
	const titlebar = document.getElementById("titlebar");
	if (titlebar === null) {
	    console.log('INTERNAL ERROR: #titlebar not found');
	    return;
	}
	titlebar.style.display = "flex";
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

    function startFullScreen(toggle) {
	if (toggle === null) {
	    console.log("Internal error: somehow called startFullScreen when no #togglefullscreen");
	    return;
	}

	const el = document.body;
	for (var name of ['requestFullScreen',
			  'mozRequestFullScreen',
			  'webkitRequestFullScreen',
			  'msRequestFullScreen']) {
	    if (name in el) {
		removeChildren(toggle);
		toggle.innerHTML = 'Normal screen';
		el[name]();
		return;
	    }
	}
	console.log("UNEXPECTED: failed to call startFullScreen");
    }

    function stopFullScreen(toggle) {
	if (toggle === null) {
	    console.log("Internal error: somehow called stopFullScreen when no #togglefullscreen");
	    return;
	}

	const el = document;
	for (var name of ['cancelFullScreen',
			  'mozCancelFullScreen',
			  'webkitCancelFullScreen',
			  'msExitFullscreen']) {
	    if (name in el) {
		removeChildren(toggle);
		toggle.innerHTML = 'Full screen';
		el[name]();
		return;
	    }
        }
	alert("Eek! Unable to cancel full screen.\nTry hitting Escape.");
    }

    var fullscreen = false;
    var entering_fullscreen = false;

    function toggleFullScreen(toggle) {
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
	fn(toggle);
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

	const toggle = document.getElementById('togglefullscreen');
	if (toggle === null) {
	    console.log("Internal error: in resize but no #togglefullscreen");
	    return;
	}

	if (fullscreen) {
	    if (entering_fullscreen) {
		entering_fullscreen = false;
	    } else {
		removeChildren(toggle);
		toggle.innerHTML = 'Full screen';
		fullscreen = false;
	    }
	} else if (toggle.innerHTML !== 'Full screen') {
	    removeChildren(toggle);
	    toggle.innerHTML = 'Full screen';
	}

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
  // - If it contains a location (from internal name lookup)
  //   use it, if it is valid
  // - get user-selected name
  // - is it a position; if so jump to it
  // - otherwise send it to the name server
  //
  function findTargetName() {
    const tgt = document.getElementById("targetName");
    var target = tgt.value;
    if (target.trim() === "") {
      // this should not happen, but just in case
      console.log("Unexpected targetName=[" + target + "]");
      return;
    }

    // Do we have a location? There is a check to ensure the target name
    // matches the recored values to allow for the user editing the field
    // (it is easier to check here rather than at each update, and it
    // allows a user to edit then go back to a known value).
    //
    const nameStr = tgt.getAttribute('data-name');
    if (nameStr === target) {
      const raStr = tgt.getAttribute('data-ra');
      const decStr = tgt.getAttribute('data-dec');
      if ((raStr !== null) && (decStr !== null)) {
	const ra = parseFloat(raStr);
	const dec = parseFloat(decStr);
	const zoom = wwt.get_fov();
	wwt.gotoRaDecZoom(ra, dec, zoom, moveFlag);
	return;
      }
    }

    console.log("Searching [" + target + "]");

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
	  wwt.gotoRaDecZoom(ra, dec, zoom, moveFlag);
	  return;
	}
      }
    }

    // Got this far, assume the input was a name to be resolved.
    //
    const host = getHost();
    if (host === null) {
      return;
    }

    const spin = spinner.createSpinner();
    host.appendChild(spin);

    // disable both; note that on success both get re-enabled,
    // which is okay because the user-entered target is still
    // in the box (ie has content), so targetFind can be enabled.
    //
    const targetName = document.getElementById('targetName');
    const targetFind = document.getElementById('targetFind');

    targetName.setAttribute('disabled', 'disabled');
    targetFind.setAttribute('disabled', 'disabled');

    lookup.lookupName(target,
		      (ra, dec) => {
			const zoom = wwt.get_fov();
			wwt.gotoRaDecZoom(ra, dec, zoom, moveFlag);
			host.removeChild(spin);

			targetName.removeAttribute('disabled');
			targetFind.removeAttribute('disabled');
		      },
		      (emsg) => reportLookupFailure(host, spin,
						    targetName, targetFind,
						    emsg));
  }

    // TODO: improve styling
    //
    function reportLookupFailure(host, spin, targetName, targetFind, msg) {

	host.removeChild(spin);

	targetName.removeAttribute('disabled');
	targetFind.removeAttribute('disabled');

	const pane = document.createElement('div');
	pane.setAttribute('class', 'lookup-failure');

	const close = addCloseButton(pane);
	pane.appendChild(close);
	
	const div = document.createElement('div');
        div.innerHTML = msg;

	pane.appendChild(div);
	host.appendChild(pane);

    }

    function initialize() {

	resize();
	
	// Load in the data
	//
	$.ajax({url: '/api/allfov', dataType: 'json'})
	    .done(parseFOVs)
	    .then(() => {
		wwt = wwtlib.WWTControl.initControl("WWTCanvas");
	        // wwt.loadImageCollection("/chandra.wtml");
		wwt.add_ready(wwtReadyFunc);
		wwt.endInit();

		/*
		 * Stop scroll events from being passed up to the browser,
		 * since it is a less-than-ideal UI. Is this the best place
		 * for it; perhaps within wwtReadyFunc?
		 */
		const canvas = document.getElementById("WWTCanvas");
		canvas.addEventListener("mousewheel",
					e => e.preventDefault());
		canvas.addEventListener("DOMMouseScroll",
					e => e.preventDefault());

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
		host.addEventListener('drop', draggable.stopDrag);

	      // Set up autocomplete
	      // - see https://stackoverflow.com/a/15713592
	      //   for over-riding the default autocomplete handler
	      //
	      // Since we allow the user to enter their own text we have
	      // to track when the names match and when they don't. This
	      // means that the #targetName keyup handler needs to handle
	      // the data fields.
	      //
	        $("#targetName").autocomplete({
		  minLength: 3,
		  source: '/api/search/name',
		  close: function(event, ui) {
		    // Do we really need this?
		    //
		    if (event.key !== "Enter") { return; }
		  },
		  select: function (event, ui) {
		    event.preventDefault();
		    this.value = ui.item.name;
		    if (ui.item.ra) {
		      this.setAttribute('data-name', ui.item.name);
		      this.setAttribute('data-ra', ui.item.ra);
		      this.setAttribute('data-dec', ui.item.dec);
		    } else {
		      this.removeAttribute('data-name');
		      this.removeAttribute('data-ra');
		      this.removeAttribute('data-dec');
		    }
		  },
		  focus: function (event, ui) {
		    event.preventDefault();
		    this.value = ui.item.name;
		    if (ui.item.ra) {
		      this.setAttribute('data-name', ui.item.name);
		      this.setAttribute('data-ra', ui.item.ra);
		      this.setAttribute('data-dec', ui.item.dec);
		    } else {
		      this.removeAttribute('data-name');
		      this.removeAttribute('data-ra');
		      this.removeAttribute('data-dec');
		    }
		  }
		})
	        .data('ui-autocomplete')._renderItem = function(ul, item) {
		    return $("<li>")
		      .append(item.name)
		      .appendTo(ul);
		};

	      // Set up zoom in/out keypresses. Should we have icons too?
	      //
	      // https://www.gavsblog.com/blog/detect-single-and-multiple-keypress-events-javascript
	      //
	      document.addEventListener('keyup', (event) => {
		if (event.key === 'z') {
		  zoomIn();
		  return;
		}
		if (event.key === 'Z') {
		  zoomOut();
		  return;
		}
	      });

	    })
	    .fail((xhr, status, e) => {
		// TODO: better error handling
		console.log("FAILED /api/allfov call");
		alert("There was a problem setting up the page.");
	    });
	
    }

    // Mapping from short to long names based on the output of
    // wwtlib.WWTControl.imageSets.map(d => d._name)
    //
    const wtml = {'2mass-image': '2Mass: Imagery (Infrared)',
                  'dss': 'Digitized Sky Survey (Color)',
                  'vlss': 'VLSS: VLA Low-frequency Sky Survey (Radio)',
                  'planck-cmb': 'Planck CMB',
                  'wise': 'WISE All Sky (Infrared)',
                  'halpha': 'Hydrogen Alpha Full Sky Map',
                  'rass': 'RASS: ROSAT All Sky Survey (X-ray)',
                  'fermi': 'Fermi LAT 8-year (gamma)'
		 };

    // Change the "background" image being displayed by WWT.
    //
    // This is "background" for the app, since WWT has its own concept
    // of foreground/background, to allow you to overlay a partially-opaque
    // foreground on the background.
    //
    // function changeBackground(background, opacity=50.0) {
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

	    const decStr = decElems.sign +
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
    }

    // Convert RA (in degrees) into hour, minutes, seconds. The
    // return value is an object.
    //
    function raToTokens(ra) {
        const hours = ra /= 15.0;

        const rah = Math.floor(hours);
        const delta = 60.0 * (hours - rah);
        const ram = Math.floor(delta);
        var ras = 60.0 * (delta - ram);
        ras = Math.floor(ras * 100.0 + 0.5) / 100.0;

	return {hours: rah, minutes: ram, seconds: ras};
    }

    // Convert Dec (in degrees) into sign, degree, minutes, seconds. The
    // return value is an object.
    //
    function decToTokens(dec) {
        const is_neg = dec < 0.0;
        const sign = (dec < 0.0) ? "-" : "+";

        const adec = Math.abs(dec);
        const decd = Math.floor(adec);
        const delta = 60.0 * (adec - decd);
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
    //
    // TODO: change this to an interval rather than a timeout
    //

    var lastLoc = "";
    var lastFOV = "";

    function showLocation() {
	// Should really cache these
	const location = document.getElementById('location-value');
	const fovloc = document.getElementById('location-fov-value');
	
	const ra = 15.0 * wwt.getRA();
	const dec = wwt.getDec();

	const newLoc = raToHTML(ra) + " " + decToHTML(dec);
	if (lastLoc !== newLoc) {
	    removeChildren(location);
	    location.innerHTML = newLoc;
	    lastLoc = newLoc;
	}

	let fov = wwt.get_fov();
	let newFOV;
	if (fov >= 1) {
	    newFOV = fov.toFixed(1) + "°";
	} else {
	    fov *= 60.0;
	    if (fov >= 1) {
		newFOV = fov.toFixed(1) + "'";
	    } else {
		fov *= 60.0;
		if (fov >= 1) {
		    newFOV = fov.toFixed(1) + "''";
		} else {
		    newFOV = "please zoom out!";
		}
	    }
	}
	if (lastFOV !== newFOV) {
	    removeChildren(fovloc);
	    fovloc.innerText = newFOV;
	    lastFOV = newFOV;
	}
    }

    // This is a bit excessive, but at least we know what the timezone is.
    // Perhaps we should return something in Chandra DOY units?
    //
    function showCurrentDate() {
        const currentDate = document.getElementById('timeline-selected-date');
        if (currentDate === null) {
	    console.log("ERROR: unable to find selector in showCurrentDate");
	    return;
        }

        $.ajax({
	    url: "/api/date",
	    cache: false,
	    dataType: "json"
        }).done(function (rsp) {
            currentDate.innerHTML = rsp;
        }).fail(function(xhr, status, e) {
	    console.log("DATE QUERY FAILED: " + status);
            currentDate.innerHTML = "";
        });
    }

    // url is the api search term to use. It can be null, but that
    // is intended for testing use only.
    //
    function addSky(url) {
	const host = getHost();
	if (host === null) { return; }

	if (url === null) {
	    console.log(" --- skipping data download");
	    makeSky(host, {title: 'Empty dataset',
			   description: 'A really empty dataset',
			   nobs: 0,
			   exptime: "no time",
			   observations: []
			  });
	    return;
	}

	const spin = spinner.createSpinner();
	host.appendChild(spin);

	d3.json(url)
	    .then((data) => {
	      host.removeChild(spin);
	      // I was doing something wrong here so explicitly catch it.
	      // This has been fixed but leave it in.
	      try {
		makeSky(host, data);
	      } catch(e) {
		// Could delete the sky pane and create an error message?
		//
		console.log("ERROR: unable to call makeSky");
		console.log(e)
		console.log("data:");
                console.log(data);
	      }
	    })
	    .catch((e) => {
		console.log("ERROR: query failed: " + url);
		console.log(e);
		host.removeChild(spin);
	    });
    }

    function textNode(txt) {
	return document.createTextNode(txt);
    }

    function em(txt) {
	const em = document.createElement('em');
	em.innerText = txt;
	return em;
    }
    function mkLink(url, content) {
	const alink = document.createElement('a');
	alink.setAttribute('href', url);
	alink.setAttribute('target', '_blank');
	alink.innerText = content;
	return alink;
    }

    // data is the JSON returned by the server: it is an object with
    // the fields:
    //   title  - title for the pane
    //   description - "long form text", optional (unclear yet)
    //   observations - a list of objects with the following attributes
    //     ra     - decimal degrees
    //     dec    - decimal degrees
    //     expks  - exposure time in ks
    //
    // We assume there will only ever be one skyPane created at a time,
    // hence we can hard-code an id. An alternative is to send one in
    // to this ruotine, but that is a *tad* more annoying to set up.
    //
    function makeSky(host, data) {

	/* delete any existing pane (rather than reusing it) */
	host.querySelectorAll('.skyPane').forEach((el) => host.removeChild(el));

	const pane = document.createElement('div');
	pane.setAttribute('class', 'skyPane');
	pane.setAttribute('id', 'skypane-identifier');

	pane.draggable = true;
	pane.addEventListener('dragstart', draggable.startDrag);

	const controlElements = document.createElement('div');
	controlElements.classList.add('controlElements');
	controlElements.classList.add('controlElementsShown');

	const title = document.createElement('span');
	title.setAttribute('class', 'title');
	title.innerText = data.title;
	controlElements.appendChild(title);

	pane.appendChild(controlElements);

	const main = document.createElement('div');
	main.setAttribute('class', 'main');

	// We do not use addCloseButton here since
	//    a) it calls resetFOV
	//    b) it only hides the pane, it doesn't destroy it,
	//       which we need to do because of the use of id
	//

	const el = document.createElement('span');
	el.setAttribute('class', 'closable');
	el.addEventListener('click', () => host.removeChild(pane));

	controlElements.appendChild(el);

	controlElements.appendChild(addHideShowButton(main, controlElements));

	pane.appendChild(main);
	host.appendChild(pane);

	if (data.description) {
	    const stypeDiv = document.createElement('div');
	    stypeDiv.setAttribute('class', 'searchtype');
	    stypeDiv.innerText = data.description;
	    main.appendChild(stypeDiv);
	}

	const nmatch = data.nobs;
	let infoText = null;
	if (nmatch == 0) {
	    infoText = 'There are no matching Chandra observations in my database!';
	} else if (nmatch == 1) {
	    infoText = 'There is one observation in my database, with an ' +
		'exposure time of ' + data.exptime + '.';
	} else {
	    infoText = 'There are ' + nmatch.toString() + ' observations ' +
		'in my database, with a total exposure time of ' +
		data.exptime + '.';
	}

	// end variable name Div even though a para
	const infoDiv = document.createElement('p');
	infoDiv.setAttribute('class', 'searchinfo');
	infoDiv.innerText = infoText;
	main.appendChild(infoDiv);

	const skyDiv = document.createElement('div');
	skyDiv.setAttribute('id', 'sky');
	main.appendChild(skyDiv);

	const details = document.createElement('details');
	const summary = document.createElement('summary');
	summary.innerText = 'About this display';
	details.appendChild(summary);

	const svgDiv = document.createElement('div');
	svgDiv.setAttribute('id', 'sky-legend');
	details.appendChild(svgDiv);

	const content = document.createElement('p');
	content.appendChild(textNode("This "));

	content.appendChild(mkLink('https://en.wikipedia.org/wiki/Orthographic_projection_in_cartography',
				   'orthographic projection'));
	content.appendChild(textNode(', using the '));
	content.appendChild(mkLink('https://en.wikipedia.org/wiki/Equatorial_coordinate_system',
				   'equatorial coordinate system'))
	
	content.appendChild(textNode(', shows a subset of Chandra observations, where each circle shows an observation - clicking on one will move the main display to that observation - and the projection can be rotated (hold down the mouse button and move it).'));
	details.appendChild(content);

        // TODO: text about the Milky Way outline
        //
        const hideDiv = document.createElement('div');
        hideDiv.setAttribute('id', 'sky-mw-buttons');

        const hideLabel = document.createElement('label');
        hideLabel.setAttribute('for', 'sky-show-mw');
        hideLabel.appendChild(textNode("Show the Milky Way?"));

        const hideButton = document.createElement('input');
        hideButton.setAttribute('id', 'sky-show-mw');
        hideButton.setAttribute('type', 'checkbox');
        hideButton.setAttribute('checked', true);
        hideButton.addEventListener('input', (event) => {
	    if(hideButton.checked){
	        sky.showMW();
	    } else {
	        sky.hideMW();
	    }
	});
        hideDiv.appendChild(hideLabel);
        hideDiv.appendChild(hideButton);
	details.appendChild(hideDiv);

	const mw = document.createElement('p');
	mw.appendChild(textNode('The outline of the plane of the Milky Way is also shown, since it helps explain some of the distribution of observations. If you are looking at Extra-galactic objects you tend to look out of the plane, and conversely most, '));
	mw.appendChild(em('but not all'));
	mw.appendChild(textNode(', objects in our Galaxy lie close to the plane.'));
	
	details.appendChild(mw);
	
	main.appendChild(details);

	const ra0 = 15.0 * wwt.getRA();
	const dec0 = wwt.getDec();
	sky.create(showObsId, ra0, dec0, data.observations);

    }

  // Do we move straight to a location or do the fancy zoom out/move/zoom in
  // process?
  //
  // If sent false then the zoom out/move/zoom in scheme is used,
  //         true       move straight there
  //
  var moveFlag = false;
  function setMoveFlag(flag) {
    moveFlag = flag;
    // saveState(keyMoveFlag, moveFlag);  -- At the moment we don't allow this to be changed
  }

  // Support zooming in or out.
  //
  // What is the minimum FOV size we want to try and "enforce"?
  // Units are degrees.
  //
  // Based on logic in
  // https://worldwidetelescope.gitbooks.io/worldwide-telescope-web-control-script-reference/content/webcontrolobjects.html#wwtcontrol-fov-property
  //
  const minFOV = 0.01; // 36 arcseconds
  const maxFOV = 60; // I think this is the WWT limit

  // Change the zoom level if it is not too small or large
  //
  function zoomTo(fov) {
    console.log(` - asked to zoom to FOV=${fov}`);
    if (fov < minFOV) { return; }
    if (fov > maxFOV) { return; }
    let ra = 15.0 * wwt.getRA();
    let dec = wwt.getDec();
    wwt.gotoRaDecZoom(ra, dec, fov, moveFlag);
  }

  const zoomFactor = 1.5;
  function zoomIn() {
    var fov = wwt.get_fov() / zoomFactor;
    zoomTo(fov);
  }

  function zoomOut() {
    var fov = zoomFactor * wwt.get_fov();
    zoomTo(fov);
  }

  /*
   * Display a Vega-Lite embedded display of the current timeline.
   * For some reason the time axis has better labels when ndays = 4
   * rather than 3.
   */

  /*
   * Store the result field so we can release it. This is done as a
   * list rather than a singleton object just in case something gets
   * messed up.
   */
  var timelineResults = [];
  var relatedResults = [];

  function showTimeline() {

      const host = getHost();
      if (host === null) { return; }

      const tl = document.getElementById('view-timeline-selector');
      if (tl === null) { return; }

      const idVal = "vl-timeline";
      var pane = document.getElementById(idVal);
      var main;
      if (pane === null) {
	  pane = document.createElement("div");
	  pane.setAttribute("id", idVal);
	  pane.setAttribute("class", "vlPane");

	  // Drag support is sub-optimal (we suddenly gain a lot more
	  // vertical space) so let's drop it for now.
	  //
	  // pane.draggable = true;
	  // pane.addEventListener('dragstart', draggable.startDrag);
	  
	  main = setupPane(pane, "Timeline");
	  host.appendChild(pane);

	  // Need to tweak the close-button.
	  //
	  const close = pane.querySelector(".closable");
	  if (close !== null) {
	      close.addEventListener('click', () => {
		  tl.innerText = "View Timeline";

		  // Remove the timeline elememnt so that we can reclaim
		  // the resources. Is this processed before or after
		  // we hide the pane (i.e. could their be flickering
		  // or other visual confusion for the user)?
		  //
		  main.querySelectorAll(".timeline").forEach((el) => {
		      main.removeChild(el);
		  });

		  // Try to restore memory/resources
		  timelineResults.forEach((result) => result.finalize());
		  timelineResults = [];

	      });
	  }

      } else {
	  // Assume this succeeds
	  main = pane.querySelector(".main");

	  // Clear out old elements
	  main.querySelectorAll(".timeline").forEach((el) => main.removeChild(el));

	  // Try to restore memory/resources
	  timelineResults.forEach((result) => result.finalize());
	  timelineResults = [];

      }
      pane.style.display = "none";

      // Short circuit the behaviour if we now we just want to hide
      // the banner (this should logically be separate).
      //
      if (tl.innerText === "Hide Timeline") {
	  tl.innerText = "View Timeline";
	  return;
      }

      const spin = spinner.createSpinner();
      host.appendChild(spin);

      const ndays = 4;
      const url = "/api/vega-lite/timeline/" + ndays;
      $.ajax({
	  url: url,
	  cache: false,
	  dataType: "json"
      }).done((rsp) => {
	  host.removeChild(spin);

	  const div = document.createElement("div");
	  div.setAttribute("class", "timeline");
	  main.appendChild(div);

	  // We select the SVG renderer so we can post-process the
	  // utput and make it reactive.
	  //
	  const opt = {renderer: 'svg'};
	  vegaEmbed(div, rsp, opt).then((result) => {

	      timelineResults.push(result);

	      pane.style.display = "block";

	      tl.innerText = "Hide Timeline";

	      // Find all the elements representing the observations and
	      // add a "link" so that they select the ObsId. This is done
	      // by assuming we store the object data within the SVG.
	      //
	      div.
		  querySelectorAll("path[aria-roledescription='bar']").
	          forEach((el) => {
		      const obsid = el.__data__.datum.obsid;
		      el.addEventListener("click", () => showObsId(obsid));
		  });

	  }).catch((err) => {
	      div.appendChild(document.createTextNode(err));
	      div.setAttribute("class", "embed-error");
	      pane.style.display = "block";
	  });

      }).fail((xhr, status, e) => {
	  host.removeChild(spin);

	  console.log(`Unable to query: ${url}`);
	  console.log(status);
	  serverGoneByBy();
      });
  }

    // This should be combined with showTimeline
    //
    const viewRelatedText = "View related observations";
    const hideRelatedText = "Hide related observations";

    const relatedSel = ".relatedlink";

    // We should only have one of these, but apparently I have not
    // been good enough in cleaning things up so there may be several
    // present, so change all of them. This is why we query the
    // document and not main. However, to try and hack around this
    // we also delete all-but-the-last element, assuming that the last
    // one is the valid one. Is there a better way to check whether
    // we still "care about" a node (by tracing up the tree and
    // checking *what*...)?
    //
    // Now, just because we return true does not mean we have changed
    // the node we are interested in, but really there should only be
    // one present anyway....
    //
    function setRelatedViewText(newText, oldText) {
	const buttons = document.querySelectorAll(relatedSel);
	const nbuttons = buttons.length;
	if (nbuttons === 0) {
	    /* not convinced this is likely so do not report to the user */
	    console.log("ERROR: no related-view button present!");
	    return;
	}

	const el = buttons[nbuttons - 1];
	var retval = false;
	if (oldText === null || el.innerText == oldText) {
	    el.innerText = newText;
	    retval = true;
	}

	if (nbuttons > 1) { console.log(`DELETING ${nbuttons - 1} buttons`); }
	// Who's forgotten all their JavaScript?
	for (var idx = 0; idx < (nbuttons - 1); idx++) {
	    const button = buttons[idx];
	    button.parentNode.removeChild(button);
	}

	return retval;
    }

  function showRelated(obsid) {

      const host = getHost();
      if (host === null) { return; }

      // Unlike the timeline case we can not guarantee this item remains valid
      // const tl = document.getElementById('view-related-selector');

      const idVal = "vl-related";

      var pane = document.getElementById(idVal);
      var main;
      if (pane === null) {
	  pane = document.createElement("div");
	  pane.setAttribute("id", idVal);
	  pane.setAttribute("class", "vlPane");

	  // Drag support is sub-optimal (we suddenly gain a lot more
	  // vertical space) so let's drop it for now.
	  //
	  // pane.draggable = true;
	  // pane.addEventListener('dragstart', draggable.startDrag);

	  main = setupPane(pane, "Related observations");
	  host.appendChild(pane);

	  // Need to tweak the close-button.
	  //
	  const close = pane.querySelector(".closable");
	  if (close !== null) {
	      close.addEventListener('click', () => {

		  setRelatedViewText(viewRelatedText, null);

		  // Remove the timeline element so that we can reclaim
		  // the resources. Is this processed before or after
		  // we hide the pane (i.e. could their be flickering
		  // or other visual confusion for the user)?
		  //
		  main.querySelectorAll(".timeline").forEach((el) => {
		      main.removeChild(el);
		  });

		  // Try to restore memory/resources
		  relatedResults.forEach((result) => result.finalize());
		  relatedResults = [];

	      });
	  }

      } else {
	  // Assume this succeeds
	  main = pane.querySelector(".main");

	  // Clear out old elements
	  main.querySelectorAll(".timeline").forEach((el) => main.removeChild(el));

	  // Try to restore memory/resources
	  relatedResults.forEach((result) => result.finalize());
	  relatedResults = [];

      }
      pane.style.display = "none";

      // Short circuit the behaviour if we now we just want to hide
      // the banner (this should logically be separate) and is a
      // bit tricker than the timeline case.
      //
      if (setRelatedViewText(viewRelatedText, hideRelatedText)) {
	  return;
      }

      const spin = spinner.createSpinner();
      host.appendChild(spin);

      const ndays = 4;
      const url = "/api/vega-lite/timeline/related/" + obsid;
      $.ajax({
	  url: url,
	  cache: false,
	  dataType: "json"
      }).done((rsp) => {
	  host.removeChild(spin);

	  const div = document.createElement("div");
	  div.setAttribute("class", "timeline");
	  main.appendChild(div);

	  // We select the SVG renderer so we can post-process the
	  // utput and make it reactive.
	  //
	  const opt = {renderer: 'svg'};
	  vegaEmbed(div, rsp, opt).then((result) => {

	      relatedResults.push(result);

	      pane.style.display = "block";

	      setRelatedViewText(hideRelatedText, null);

	      // Find all the elements representing the observations and
	      // add a "link" so that they select the ObsId. This is done
	      // by assuming we store the object data within the SVG.
	      //
	      div.
		  querySelectorAll("path[aria-roledescription='circle']").
	          forEach((el) => {
		      const obsid = el.__data__.datum.obsid;
		      el.addEventListener("click", () => showObsId(obsid));
		  });

	  }).catch((err) => {
	      div.appendChild(document.createTextNode(err));
	      div.setAttribute("class", "embed-error");
	      pane.style.display = "block";
	  });

      }).fail((xhr, status, e) => {
	  host.removeChild(spin);

	  console.log(`Unable to query: ${url}`);
	  console.log(status);
	  serverGoneByBy();
      });
  }

    return {initialize: initialize,
	    resize: resize,

	    // make it easier to debug/explore things
	    showObsId: showObsId,
	    changeBackground: changeBackground,

	    // Do we still need these?
	    getWWT: () => wwt,
	    getHost: getHost,

	    // See app/API.hs for calls to this
	    addSkyView: addSky,

	    zoomIn : zoomIn,
	    zoomOut : zoomOut,

	    showTimeline: showTimeline
	    
	   };

})();

// end
