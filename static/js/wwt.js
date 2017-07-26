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

    function toggleCrosshairs() {
        displayCrosshairs = !displayCrosshairs;
        wwt.settings.set_showCrosshairs(displayCrosshairs);
    }

    function toggleConstellations() {
        displayConstellations = !displayConstellations;
        wwt.settings.set_showConstellationFigures(displayConstellations);
    }

    function toggleBoundaries() {
        displayBoundaries = !displayBoundaries;
        wwt.settings.set_showConstellationBoundries(displayBoundaries);
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
        base.hide_class("nowwt");
        wwt.add_ready(wwtReadyFunc(obsdata));
        wwt.endInit();
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
