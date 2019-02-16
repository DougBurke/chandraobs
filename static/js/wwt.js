/*
 * Based on code from
 * http://www.worldwidetelescope.org/docs/Samples/displaycode.htm?codeExample=WWTWebClientPolyHtml5.html
 * and
 * http://www.worldwidetelescope.org/docs/worldwidetelescopewebcontrolscriptreference.html
 *
 */

"use strict";

var wwt = (function (base) {
    
    var wwt;
    var displayCrosshairs = true;
    var displayConstellations = true;
    var displayBoundaries = true;
    var displayNearbyFOVs = true;

    /*
     * No longer trying to store the position
     * var raPos, decPos;
     */

    var startFOV = 5;

    // Store the polygons for the FOV regions:
    //  - the selected observation
    //  - nearby observations
    //
    var fovAnnotation;
    var nearbyFOVs = [];
    var nearbyObsData = [];

    var highlightedFOV = null;
    
    // color of the other FOVs and of the user-selected FOV
    const otherFOVcolor = 'gray';
    const highlightedFOVcolor = 'orange';
    
    const paneMimeType = "application/x-pane+json";

    // true means that the area has been collapsed, false means that it
    // is being shown
    var resize_state = true;
    
    // Returns the new setting
    function toggleCrosshairs() {
        displayCrosshairs = !displayCrosshairs;
        wwt.settings.set_showCrosshairs(displayCrosshairs);
        return displayCrosshairs;
    }

    function toggleConstellations() {
        displayConstellations = !displayConstellations;
        wwt.settings.set_showConstellationFigures(displayConstellations);
        return displayConstellations;
    }

    function toggleBoundaries() {
        displayBoundaries = !displayBoundaries;
        wwt.settings.set_showConstellationBoundries(displayBoundaries);
        return displayBoundaries;
    }

    function toggleNearbyFOVs() {
        displayNearbyFOVs = !displayNearbyFOVs;

	// As can take some time, would like to set up a spinner
	// here, or some-other means of indicating to the user that
	// work is being done.
	//
	if (displayNearbyFOVs) {
	    showNearbyFOVs();
	} else {
	    hideNearbyFOVs();
	}

        return displayNearbyFOVs;
    }

    // Used by click handlers to set the label appropriately and call the
    // action (which has no arguments).
    //
    function handleToggle(target, label, toggleAction) {
        const newval = toggleAction();
        let action;
        if (newval) {
            action = "Hide ";
        } else {
            action = "Show ";
        }
        target.innerHTML = action + label;
    }
    
    /*
      function toggleFOV() {
      displayFOV = !displayFOV;
      if (displayFOV) {
      wwt.addAnnotation(fovAnnotation);
      } else {
      wwt.removeAnnotation(fovAnnotation);
      }
      }
    */

    /*
    function resetLocation() {
        var fov = wwt.get_fov();
        wwt.gotoRaDecZoom(raPos, decPos, fov, false);
    }
    */

    // Return settings for the FOV display based on the
    // observational status.
    //
    // Return the color to use for the FOV; this depends on
    // whether the observation has been done, is running,
    // or is yet to be observed. The same colors as the
    // main display can be used.
    //
    // The "running" color should be rgba(245,245,245,0.8)
    // aka #F5F5F5 but we currently don't have that information
    //
    // The linewidth depends on if this is a "real" observation
    // or an unobserverd or discarded one (thinner).
    //
    // This needs review now that we are also adding nearby
    // observations to the display (as some of this work is
    // wasted).
    //
    function fovSettings(obsdata) {
        let color, linewidth;
        linewidth = 2;
        if ((obsdata.status === "archived") ||
            (obsdata.status === "observed")) {
            // return "rgba(216,179,101,0.8)";
            color = "#D8B365";
        } else if (obsdata.status === "scheduled") {
            // return "rgba(90,180,172,0.8)";
            color = "#5AB4AC";
        } else if (obsdata.status === "unobserved") {
            linewidth = 1;
            color = "yellow";
        } else {
            // discarded or canceled
            linewidth = 1;
            color = "red";
        }
        return {color: color, linewidth: linewidth};
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

    // Query for nearby FOVs and display them
    //
    function addNearbyFOV(obsdata) {

	/***
	$.ajax({url: '/api/nearbyfov',
		data: {obsid: obsdata.obsid,
		       ra: obsdata.ra,
		       dec: obsdata.dec
		      },
		dataType: 'json'})
        ***/

	// if do go with allfov may want to exclude obsid from the list
	// when displaying them
	//
	$.ajax({url: '/api/allfov',
		dataType: 'json'})
	
	    .done((rsp) => { addFOVs(obsdata, rsp); })
	    .fail((xhr, status, e) => {
		console.log("FAILED nearbyfov call");
	    })
	    .always(() => {
		// draw current FOV after the others so it's on top.
		//
		fovAnnotation = addFOV(obsdata, true);
            });
    }

    // We do not add in the current FOV here since we want to always
    // do that, even if the "get other FOV" query fails.
    //
    function addFOVs(obsdata0, rsp) {
	// Only show the toggle option if we have any nearby FOVs
	if (rsp.length === 0) { return; }

	// Making this appear is visually distracting, since it makes
	// the container expand to fit the new content. It would perhaps
	// be nice to animate this - although if we switch to showing
	// all FOVs then we can assume there's always going to be a
	// button so no need to show it here.
	//
	/***
	document.getElementById('toggleNearbyFOVs')
	    .style.display = 'block';
	***/
	
	// We want to exclude the current observation
	//
	rsp.forEach((obsdata) => {
	    if (obsdata.obsid !== obsdata0.obsid) {
		nearbyFOVs.push(addFOV(obsdata, false));
		nearbyObsData.push(obsdata);
	    }
	});

	// It would make things a lot easier if the annotation Clicked
	// event really was fired.
	//
	/***
	wwt.add_annotationClicked((obj, eventArgs) => {
	    console.log("----> annotation clicked");
	});
	***/

	// only add the click support if other observations are loaded
	//
	wwt.add_clicked((obj, eventArgs) => {
	    let ra = eventArgs.get_RA();
	    if (ra < 0) { ra += 360.0; }
	    const dec = eventArgs.get_dec();
	    identifyNearestObsIds(obsdata0, ra, dec);
	});
    }

    // Could be lazy and just call the server to find out the nearest
    // observations, excluding the current observation.
    //
    function identifyNearestObsIds(obsdata0, ra, dec) {

	// unhighlight any previously-selected FOV
	//
	if (highlightedFOV !== null) {
	    highlightedFOV.set_lineColor(otherFOVcolor);
	    highlightedFOV = null;
	}

	const host = document.getElementById('obspane');
	if (host === null) {
	    console.log("INTERNAL ERROR: no #obspane element");
	}
	
	// Search out to the approximate screen size
	const maxSep = wwt.get_fov();

	var seps = [];
	for (var i = 0; i < nearbyObsData.length; i++) {
	    const obsdata = nearbyObsData[i];
	    if (obsdata.obsid === obsdata0.obsid) { return; }
	    
	    const sep = separation(ra, dec, obsdata.ra, obsdata.dec);
	    if (sep <= maxSep) {
		seps.push([sep, i]);
	    }
	}

	// Hide the previous obs info, if set.
	if (seps.length === 0) {
	    if (host !== null) {
		host.style.display = 'none';
	    }
	    return;
	}
	
	// Sort on the separation (first item; could sort by name or obsid
	// as a secondary step but not sure it's worth it).
	//
	seps.sort(function (a, b) { if (a[0] < b[0]) { return -1; }
				    else if (a[0] > b[0]) { return 1; }
				    else { return 0; } });
    
	
        // Highlight selected FOV
        //
	const idx = seps[0][1];
	highlightedFOV = nearbyFOVs[idx];
	highlightedFOV.set_lineColor(highlightedFOVcolor);

	if (host !== null) {
	    addObsPane(host, nearbyObsData[idx]);
	}
    }

    // Hide the obsid display, if set
    //
    function hideNearbyFOVs() {
	const host = document.getElementById('obspane');
	if (host !== null) {
	    host.style.display = 'none';
	}

	nearbyFOVs.forEach((fov) => {
	    wwt.removeAnnotation(fov);
	});
    }

    // Show the obsid display if selected
    //
    function showNearbyFOVs() {
	wwt.removeAnnotation(fovAnnotation);
	nearbyFOVs.forEach((fov) => {
	    wwt.addAnnotation(fov);
	});
	wwt.addAnnotation(fovAnnotation);

	if (highlightedFOV !== null) {
	    const host = document.getElementById('obspane');
	    if (host !== null) {
		host.style.display = 'block';
	    }
	}
    }

    // TODO: remove the highlighted FOV
    //
    function clearHighlightedObs() {
	const host = document.getElementById('obspane');
	if (host === null) { return; }

	host.style.display = 'none';

	if (highlightedFOV !== null) {
	    highlightedFOV.set_lineColor(otherFOVcolor);
	    highlightedFOV = null;
	}
    }
    
    // Add a panel providing information on the selected observation
    //
    function addObsPane(host, obsdata) {
	var desc = '';

	const cleanName = encodeURIComponent(obsdata.name);
	
	// Could link to a bunch of different information here, but for now
	// keep it simple.
	//
        desc += "<span class='clickable' onclick='wwt.clearHighlightedObs();'></span>";

	desc += '<table><tbody>';
	desc += '<tr><td>Target:</td><td>' +
	    '<a href="/search/name?target=' + cleanName + '">' + obsdata.name + '</td></tr>';
	desc += '<tr><td>ObsId:</td><td>';
	desc += '<a href="/obsid/' + obsdata.obsid + '">' +
	    obsdata.obsid + '</a></td></tr>';
	desc += '<tr><td>Instrument:</td><td>' + obsdata.instrument + '</td></tr>';
	desc += '<tr><td>Status:</td><td>' + obsdata.status + '</td></tr>';
	desc += '</tbody></table>'

	host.innerHTML = desc;

	// ensure the border matches the color used for the highlighting, although
	// transparency/alpha setting likely to be different.
	//
	host.style.borderColor = highlightedFOVcolor;
	
	host.style.display = 'block';
    }
    
    // draw on the FOV
    //
    function addFOV(obsdata, current=false) {

        const settings = fovSettings(obsdata);

        const points = shiftFOV(obsdata);
        const fov = wwt.createPolyLine(true);

        // Unfortunately these do not do much with the WWT
        // at present, but left in for now.
        //
        fov.set_id("ObsId " + obsdata.obsid);
        fov.set_label(obsdata.name);
        fov.set_showHoverLabel(true);

        // fov.set_lineColor("0x8800FFFF");
        // fov.set_lineColor("green");

        // fov.set_lineColor(settings.color);
        // fov.set_lineWidth(settings.linewidth);

	if (current) {
            fov.set_lineColor(settings.color);
            fov.set_lineWidth(4);
            fov.set_opacity(1.0);

	} else {
            fov.set_lineColor(otherFOVcolor);
	    // fov.set_lineWidth(settings.linewidth);
	    fov.set_lineWidth(2);

	    // use the separation to determine the opacity;
	    // for now a linear scaling from the center, and assume
	    // that separation is 0 to 1. Perhaps should ensure a
	    // minimum opacity (rather than allow to go to 0)?
	    //
            // fov.set_opacity(0.8 * (1.0 - obsdata.separation));

	    fov.set_opacity(0.5); // for ALL fovs
	}

        for (const p of points) {
            fov.addPoint(p[0], p[1]);
        }
        wwt.addAnnotation(fov);

	return fov;
    }

    function wwtReadyFunc(obsdata) {
        return function () {
            wwt.settings.set_showCrosshairs(displayCrosshairs);
            wwt.settings.set_showConstellationFigures(displayConstellations);
            wwt.settings.set_showConstellationBoundries(displayBoundaries);

	    // coordinate grid
	    wwt.settings.set_showEquatorialGridText(true);
	    wwt.settings.set_showGrid(true);

            wwt.hideUI(true);
	    addNearbyFOV(obsdata);

            wwt.gotoRaDecZoom(obsdata.ra, obsdata.dec, startFOV, false);
        };
    }

    function initialize(obsdata) {
        base.hide_nojs();
        wwt = wwtlib.WWTControl.initControl("WWTCanvas");

        wwt.loadImageCollection("chandra.wtml");
        
        base.hide_class("nowwt");
        wwt.add_ready(wwtReadyFunc(obsdata));
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
        canvas.onmouseout = e => wwtlib.WWTControl.singleton.onMouseUp(e);

        // Set up the handlers for the control buttons
        //
        document.getElementById("toggleConstellation")
            .addEventListener("click", (e) => {
                handleToggle(e.target, "Constellations", toggleConstellations);
            });
        document.getElementById("toggleBoundaries")
            .addEventListener("click", (e) => {
                handleToggle(e.target, "Boundaries", toggleBoundaries);
            });
        document.getElementById("toggleNearbyFOVs")
            .addEventListener("click", (e) => {
                handleToggle(e.target, "other FOVs", toggleNearbyFOVs);
            });

        // Handle the show/hide button. The click handler is assigned
        // to the div containing the two buttons.
        //
        const hide = document.getElementById("resizeHide");
        const show = document.getElementById("resizeShow");

        const contents = document.getElementById("resizeWWTArea");
        hide.style.display = "block";
        show.style.display = "none";

        document.getElementById("resizeButtons")
            .addEventListener("click", () => {
                let state;
                if (resize_state) {
                    hide.style.display = "none";
                    show.style.display = "block";
                    state = "none";
                } else {
                    hide.style.display = "block";
                    show.style.display = "none";
                    state = "block";
                }
                contents.style.display = state;
                resize_state = !resize_state;
            });
        

        // Set up the drag-and-drop handlers
        //
        // TODO
        canvas.addEventListener("drop", (e) => { dropPane(e); })
        canvas.addEventListener("dragover", (e) => { e.preventDefault(); })

        document.getElementById("wwtusercontrol")
            .addEventListener("dragstart", (e) => { dragStartPane(e); });
        document.getElementById("obspane")
            .addEventListener("dragstart", (e) => { dragStartPane(e); });

        document.getElementById("imagechoice")
            .addEventListener("change", (e) => { setImage(e.target.value); });
    }

    // Perhaps I should just always use clientX/Y rather than this?
    //
    function getEventPos(event) {
        let x, y;
        if (typeof event.x === "undefined") {
            x = event.clientX;
            y = event.clientY;
        } else {
            x = event.x;
            y = event.y;
        }
        return {x: x, y: y};
    }

    /*
     * Handling of draggable "windows" on the canvas, so called "panes".
     */
    function dragStartPane(event) {

        const toMove = event.target.id;
        
        // Store the current location of the event, so we can find out
        // how much we have shifted by.
        //
        const evpos = getEventPos(event);
        evpos.id = toMove;
        event.dataTransfer.setData(paneMimeType, JSON.stringify(evpos));

        // The id is not supposed to contain a space, and we don't, so
        // it's okay to separate with spaces.
        //
        const store = evpos.x.toString() + " " + evpos.y.toString() +
            " " + toMove;
        event.dataTransfer.setData("text/plain", store);

        // All we want to do is change the position of the div,
        // but do not change the DOM at all. So it's not clear
        // what the best value here is.
        //
        event.dataTransfer.effectAllowed = "move";
        event.dataTransfer.dropEffect = "move";
    }
    
    function dropPane(event) {

        // TODO: check for invalid data or failure to parse.
        //
        // It's not clear whether I can guarantee that the structured
        // data is available, so code up a fallback, just in case.
        //
        let x0, y0, toMove;
        let fallback = true;
        for (const t of event.dataTransfer.types) {
            if (t === paneMimeType) {
                fallback = false;
                break;
            }
        }
        if (fallback) {
            let store = event.dataTransfer.getData('text/plain');
            let tags = store.split(" ");
            if (tags.length !== 3) {
                console.log("Invalid store data from drop: [" + store + "]");
                event.preventDefault();  // is this sensible here?
                return;
            }
            x0 = parseInt(tags[0]);
            y0 = parseInt(tags[1]);
            toMove = tags[2];
        } else {
            let json = event.dataTransfer.getData(paneMimeType);
            let obj = JSON.parse(json);
            x0 = obj.x;
            y0 = obj.y;
            toMove = obj.id;
        }
            
        let evpos = getEventPos(event);
        let dx = evpos.x - x0;
        let dy = evpos.y - y0;

        let div = document.getElementById(toMove);
        let stackbbox = div.getBoundingClientRect();
        let parentbbox = div.parentElement.getBoundingClientRect();

        let x1 = div.offsetLeft + dx;
        let y1 = div.offsetTop + dy;

        let xmin = 0, xmax = parentbbox.width - stackbbox.width;
        let ymin = 0, ymax = parentbbox.height - stackbbox.height;
        if (x1 < xmin) { x1 = xmin; } else if (x1 > xmax) { x1 = xmax; }
        if (y1 < ymin) { y1 = ymin; } else if (y1 > ymax) { y1 = ymax; }

        div.style.left = x1.toString() + "px";
        div.style.top = y1.toString() + "px";

        event.preventDefault();

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

    /*
     * Special case the DSS image, since can just hide the foreground image
     * in this case, which saves time.
     */
    function setImage(name) {
        if (name === "dss") {
            wwt.setForegroundOpacity(0.0);
            return;
        }
        const fullName = wtml[name];
        if (typeof fullName === "undefined") {
            console.log("Unknown name: " + name);
            return;
        }
        wwt.setForegroundImageByName(fullName);
        wwt.setForegroundOpacity(100.0);
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
    /*
     * Sets the location of WWT, including any intialization that may
     * need to take place. It is assumed to be called when the
     * WWT "view" is selected (i.e. shown to the user).
     *
     * The initialization is done here so it is "on demand" rather
     * than whenever the page loads, as it is a bit heavyweight
     * (this is not the main view on the page)
     *
     * The argument should be an object with the following
     * fields: ra, dec, roll, instrument, and name.
     */
    var isInitialized = 0;
    function setLocation(obsdata) {
        // console.log("* set-location isInitialized=" + isInitialized);
        if (isInitialized === 0) {
            // console.log("--> initializing WWT");
            initialize(obsdata);
            isInitialized = 1;
        } else {
            const fov = startFOV; // do we want to use the current zoom?
            // console.log("* zoom level = " + fov);
            wwt.gotoRaDecZoom(obsdata.ra, obsdata.dec, fov, false);
        }
        // console.log("* obsdata = "); console.log(obsdata);
    }

    /*
     * For the main2 view, the WWT controls are currently destroyed
     * when switching between observations, so we need to clear the
     * initialization flag when this happens.
     */
    function resetStatus() {
        // console.log("** resetStatus: initialized was " + isInitialized);
        isInitialized = 0;
    }
    
    return {initialize: initialize,
            // resetLocation: resetLocation,
            toggleCrosshairs: toggleCrosshairs,
            toggleConstellations: toggleConstellations,
            toggleBoundaries: toggleBoundaries,

            // newer API
            setLocation: setLocation,
            resetStatus: resetStatus,

	    clearHighlightedObs: clearHighlightedObs,
	    
           };
    
})(base);
