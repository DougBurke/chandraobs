/*
 * Based on code from
 * http://www.worldwidetelescope.org/docs/Samples/displaycode.htm?codeExample=WWTWebClientPolyHtml5.html
 * and
 * http://www.worldwidetelescope.org/docs/worldwidetelescopewebcontrolscriptreference.html
 *
 * note that wwt.goto() is actually wwt.gotoRaDecZoom()
 */

"use strict";

var dummy;

var wwt = (function (base) {
    
    var wwt;
    var displayCrosshairs = true;
    var displayConstellations = true;
    var displayBoundaries = true;

    /* var displayFOV = true; */

    /*
     * No longer trying to store the position
     * var raPos, decPos;
     */

    var startFOV = 5;
    // var fovAnnotation;

    var paneMimeType = "application/x-pane+json";

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

	$.ajax({url: '/api/nearbyfov',
		data: {obsid: obsdata.obsid,
		       ra: obsdata.ra,
		       dec: obsdata.dec
		      },
		dataType: 'json'})
	    .done(addFOVs)
	    .fail((xhr, status, e) => {
		console.log("FAILED nearbyfov call");
	    })
	    .always(() => {
		// draw current FOV on top
		addFOV(obsdata, true);
            });
    }

    function addFOVs(rsp) {
	rsp.forEach((obsdata) => { addFOV(obsdata, false); });
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
            fov.set_lineColor('gray');
	    // fov.set_lineWidth(settings.linewidth);
	    fov.set_lineWidth(2);

	    // use the separation to determine the opacity;
	    // for now a linear scaling from the center, and assume
	    // that separation is 0 to 1. Perhaps should ensure a
	    // minimum opacity (rather than allow to go to 0)?
	    //
            fov.set_opacity(0.8 * (1.0 - obsdata.separation));
	}

        for (const p of points) {
            fov.addPoint(p[0], p[1]);
        }
        wwt.addAnnotation(fov);
        // fovAnnotation = fov;
    }

    function wwtReadyFunc(obsdata) {
        // raPos = obsdata.ra;
        // decPos = obsdata.dec;
        return function () {
            wwt.settings.set_showCrosshairs(displayCrosshairs);
            wwt.settings.set_showConstellationFigures(displayConstellations);
            wwt.settings.set_showConstellationBoundries(displayBoundaries);
            wwt.hideUI(true);
	    addNearbyFOV(obsdata);
            // addFOV(obsdata, true);
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
            resetStatus: resetStatus

           };
    
})(base);
