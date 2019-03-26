"use strict";

// Astronomers like to do things backwards. This includes displaying
// the night sky. There are reasons for this, but it complicates things.
//
// This module provides two routines:
//
//   toLonLat - convert ra/dec (decimal degrees) to lon/lat
//
//   invertXProjection - convert a d3 projection to invert the X axis
//
//

const raProjection = (() => {

    // Return an array of longitude, latitude values given
    // RA and Dec in decimal degrees.
    //
    function toLonLat(ra, dec) {
	if (ra > 180) {
	    return [ra - 360.0, dec];
	} else {
	    return [ra, dec];
	}
    }

    // Given a "raw" projection - e.g. d3.geoOrthographicRaw -
    // return a projection "instance" that inverts the X axis.
    //
    // I have tried to cobine projections via the stream interface
    // and d3.geoIdentity().reflectX(true), but failed so am
    // using this approach, which is based on the d3-celestial
    // project - in particular
    // https://github.com/ofrohn/d3-celestial/blob/master/src/projection.js
    // but updated to d3 v5.
    //
    function invertX(rawProj) {
	if ((typeof rawProj === "undefined") ||
	    (typeof rawProj.invert === "undefined") ||
	    (rawProj.invert === null)) {
	    console.log("ERROR: invertX not sent a d3.geo*Raw?");
	    return null;
	}
	
	const projection = (lon, lat) => {
	    return rawProj(-lon, lat);
	};

	projection.invert = (x, y) => {
	    try {
		const pos = rawProj.invert(x, y);
		return [-pos[0], pos[1]];
	    } catch(e) {
		// Does it make sense to return anything here?
		console.log("Unable to invert projection");
		return null;
	    }
	}

	return d3.geoProjection(projection);
    }
    
    return { toLonLat: toLonLat,
	     invertXProjection: invertX
	   };

})();
