/*
 * Based on code from
 * http://www.worldwidetelescope.org/docs/Samples/displaycode.htm?codeExample=WWTWebClientPolyHtml5.html
 * and
 * http://www.worldwidetelescope.org/docs/worldwidetelescopewebcontrolscriptreference.html
 *
 * note that wwt.goto() is actually wwt.gotoRaDecZoom()
 */

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
    var fovAnnotation;

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
        var newval = toggleAction();
        var action;
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

    // Hack up an approximate field of view; this does
    // not account for any offset between the given
    // position and the aimpoint of the instrument.
    //
    // TODO: handle overlap at ra=0/360 boundary?
    //
    function shiftFOV(obsdata) {

        var ra = obsdata.ra;
        var dec = obsdata.dec;
        var roll = obsdata.roll;
        var instrument = obsdata.instrument;
        
        // assume center is at 0,0; units are in degrees,
        // no roll.
        var aw = 8.0/60.0; // width of ACIS chip in degrees
        var hiw = 30.0/60.0; // width of HRC-I chip in degrees
        var aw2 = aw/2;
        var hiw2 = hiw/2;
        var hsw = 99.0 / 60.0;
        var hsh = 6.0 / 60.0; // if use 3.4 then need to deal with offset
        var hsw2 = hsw / 2;
        var hsh2 = hsh / 2;
        var p0;
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
        
        var p1 = new Array(p0.length);

        // how is roll defined? clockwise? where is roll=0?
        // It seems that this is correct by comparing to the
        // DSS result
        var ang = roll * Math.PI / 180.0;
        var cosang = Math.cos(ang);
        var sinang = Math.sin(ang);
        var cosdec = Math.cos(dec * Math.PI / 180.0);
        
        for (var i = 0; i < p0.length; i++) {
            var x0 = p0[i][0];
            var y0 = p0[i][1];

            var dx = (x0 * cosang - y0 * sinang) / cosdec;
            var x1 = ra + dx;
            var y1 = dec + x0 * sinang + y0 * cosang;

            p1[i] = [x1, y1];
        }
        return p1;
    }

    function addFOV(obsdata) {
        
        var points = shiftFOV(obsdata);
        var fov = wwt.createPolyLine(true);
        fov.set_id("fov");
        fov.set_label(obsdata.name);
        fov.set_showHoverLabel(true);
        //fov.set_lineColor("0x8800FFFF");
        fov.set_lineColor("green");
        fov.set_lineWidth(1);
        //fov.set_opacity(0.6);
        fov.set_opacity(1.0);
        for(var i in points) {
            fov.addPoint(points[i][0], points[i][1]);
        }
        wwt.addAnnotation(fov);
        fovAnnotation = fov;
    }

    
    function wwtReadyFunc(obsdata) {
        raPos = obsdata.ra;
        decPos = obsdata.dec;
        return function () {
            wwt.settings.set_showCrosshairs(displayCrosshairs);
            wwt.settings.set_showConstellationFigures(displayConstellations);
            wwt.settings.set_showConstellationBoundries(displayBoundaries);
            wwt.hideUI(true);
            addFOV(obsdata);
            wwt.gotoRaDecZoom(raPos, decPos, startFOV, false);
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
        var canvas = document.getElementById("WWTCanvas");
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
        var hide = document.getElementById("resizeHide");
        var show = document.getElementById("resizeShow");
        var contents = document.getElementById("resizeWWTArea");
        hide.style.display = "block";
        show.style.display = "none";

        document.getElementById("resizeButtons")
            .addEventListener("click", () => {
                var state;
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
        var x, y;
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

        var toMove = event.target.id;
        
        // Store the current location of the event, so we can find out
        // how much we have shifted by.
        //
        var evpos = getEventPos(event);
        evpos.id = toMove;
        event.dataTransfer.setData(paneMimeType, JSON.stringify(evpos));

        // The id is not supposed to contain a space, and we don't, so
        // it's okay to separate with spaces.
        //
        var store = evpos.x.toString() + " " + evpos.y.toString() +
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
        var x0, y0, toMove;
        var fallback = true;
        var types = event.dataTransfer.types;
        for (var i=0; i < types.length; i++) {
            if (types[i] === paneMimeType) {
                fallback = false;
                break;
            }
        }
        if (fallback) {
            var store = event.dataTransfer.getData('text/plain');
            var tags = store.split(" ");
            if (tags.length !== 3) {
                console.log("Invalid store data from drop: [" + store + "]");
                event.preventDefault();  // is this sensible here?
                return;
            }
            x0 = parseInt(tags[0]);
            y0 = parseInt(tags[1]);
            toMove = tags[2];
        } else {
            var json = event.dataTransfer.getData(paneMimeType);
            var obj = JSON.parse(json);
            x0 = obj.x;
            y0 = obj.y;
            toMove = obj.id;
        }
            
        var evpos = getEventPos(event);
        var dx = evpos.x - x0;
        var dy = evpos.y - y0;

        var div = document.getElementById(toMove);
        var stackbbox = div.getBoundingClientRect();
        var parentbbox = div.parentElement.getBoundingClientRect();

        var x1 = div.offsetLeft + dx;
        var y1 = div.offsetTop + dy;

        var xmin = 0, xmax = parentbbox.width - stackbbox.width;
        var ymin = 0, ymax = parentbbox.height - stackbbox.height;
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
        var fullName = wtml[name];
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
     * The initialization is done here so it si "on demand" rather
     * than whenever the page loads, as it is a bit heavyweight.
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
            var fov = startFOV; // do we want to use the current zoom?
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
