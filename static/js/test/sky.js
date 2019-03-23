"use strict";

//
// Create a projection of the sky using D3's orthographic
// projection. Based in part on http://bl.ocks.org/pnavarrc/9730300
// an https://jorin.me/d3-canvas-globe-hover/ (although currently not
// using the latter).
//
// https://d3indepth.com/geographic/ is also useful
//

// Would like the drag behavior from https://www.jasondavies.com/maps/rotate/
// and I use http://bl.ocks.org/ivyywang/7c94cb5a3accd9913263
// to approximate it.
// Should perhaps move to torsor representation at
//    https://bl.ocks.org/HarryStevens/75b3eb474527c10055618fa00123ba44
// (same thing but hiding logic in an external package)
//

// TODO:
//   - add in circles showing the range of exposures?
//

// Requires that d3 is loaded.
//
const sky = (function() {

    const width = 400;    
    const height = 400;

    const to_radians = Math.PI / 180;
    const to_degrees = 180 / Math.PI;

    // Try and improve the drag behavior based on
    // http://bl.ocks.org/ivyywang/7c94cb5a3accd9913263
    //
    function dragSphere(svg, path, projection) {

	function cross(v0, v1) {
	    return [v0[1] * v1[2] - v0[2] * v1[1], v0[2] * v1[0] - v0[0] * v1[2], v0[0] * v1[1] - v0[1] * v1[0]];
	}

	function dot(v0, v1) {
	    for (var i = 0, sum = 0; v0.length > i; ++i) sum += v0[i] * v1[i];
	    return sum;
	}

	function lonlat2xyz( coord ){
	    const lon = coord[0] * to_radians;
	    const lat = coord[1] * to_radians;
	    
	    const x = Math.cos(lat) * Math.cos(lon);
	    const y = Math.cos(lat) * Math.sin(lon);
	    const z = Math.sin(lat);
	    return [x, y, z];
	}

	function quaternion(v0, v1) {
	    if (v0 && v1) {
		
		const w = cross(v0, v1),  // vector pendicular to v0 & v1
	              w_len = Math.sqrt(dot(w, w)); // length of w     

		if (w_len == 0) {
        	    return null;
		}
	    
		const theta = 0.5 * Math.acos(Math.max(-1, Math.min(1, dot(v0, v1))));
	    
		const qi  = w[2] * Math.sin(theta) / w_len; 
		const qj  = - w[1] * Math.sin(theta) / w_len; 
		const qk  = w[0] * Math.sin(theta) / w_len;
		const qr  = Math.cos(theta);

		// TODO: what is JS doing here???
		return theta && [qr, qi, qj, qk];
	    } else {
		return null;
	    }
	}

	function euler2quat(e) {
	    if(!e) { return null };
    
	    const roll = .5 * e[0] * to_radians;
            const pitch = .5 * e[1] * to_radians;
            const yaw = .5 * e[2] * to_radians;
	    
            const sr = Math.sin(roll);
            const cr = Math.cos(roll);
            const sp = Math.sin(pitch);
            const cp = Math.cos(pitch);
            const sy = Math.sin(yaw);
            const cy = Math.cos(yaw);
	    
            const qi = sr*cp*cy - cr*sp*sy;
            const qj = cr*sp*cy + sr*cp*sy;
            const qk = cr*cp*sy - sr*sp*cy;
            const qr = cr*cp*cy + sr*sp*sy;
	
	    return [qr, qi, qj, qk];
	}

	function quatMultiply(q1, q2) {
	    if(!q1 || !q2) { return null; }

	    const a = q1[0];
            const b = q1[1];
            const c = q1[2];
            const d = q1[3];
            const e = q2[0];
            const f = q2[1];
            const g = q2[2];
            const h = q2[3];
	
	    return [ a*e - b*f - c*g - d*h,
		     b*e + a*f + c*h - d*g,
		     a*g - b*h + c*e + d*f,
		     a*h + b*g - c*f + d*e
		   ];
	}

	function quat2euler(t){
	    if(!t) { return null; }

	    return [ Math.atan2(2 * (t[0] * t[1] + t[2] * t[3]),
				1 - 2 * (t[1] * t[1] + t[2] * t[2])) * to_degrees, 
		     Math.asin(Math.max(-1, Math.min(1, 2 * (t[0] * t[2] - t[3] * t[1])))) * to_degrees, 
		     Math.atan2(2 * (t[0] * t[3] + t[1] * t[2]),
				1 - 2 * (t[2] * t[2] + t[3] * t[3])) * to_degrees
		   ];
	}

	function eulerAngles(v0, v1, o0) {
	    const t = quatMultiply(euler2quat(o0),
				   quaternion(lonlat2xyz(v0), lonlat2xyz(v1)));
	    return quat2euler(t);	
	}

	let posStart = null;
	function dragStart() {
	    posStart = projection.invert([d3.event.x,
					  d3.event.y]);
	    svg.insert("path")
		.datum({type: "Point",
			coordinates: posStart,
			properties: { exposure: 10 }
		       })
		.attr("class", "dragpoint")
		.attr("d", path);
	}
    
	function dragEnd() {
	    svg.selectAll('.dragpoint').remove();
	    posStart = null;
	}
    
	function drag() {
	    const posCurrent = projection.invert([d3.event.x,
						  d3.event.y]);
	    const r = projection.rotate();
		
	    const rnew = eulerAngles(posStart, posCurrent, r);
	    if (rnew !== null) {
		projection.rotate(rnew);
		svg.selectAll('path').attr('d', path);
	    }
	}

	const dragObj = d3.drag()
	      .on('start', dragStart)
	      .on('drag', drag)
	      .on('end', dragEnd);

	return { drag: dragObj,
		 inDrag: () => { return posStart !== null; }
	       };
    }
    
    // time for a transition, in milliseconds
    // const transitionTime = 600;

    // Highlight the given observation (mouseover)
    //
    // It would be nice to be able to transition between styles,
    // to separate code from configuration.
    //
    function selectObs(inDrag) {
	return (d) => {
	    if (inDrag()) { return ; }
            const sel = '#obs-' + d.properties.obsid;
	    /***
		d3.selectAll(sel).transition()
		.duration(transitionTime)
		.style('fill', '#c33')
		.style('fill-opacity', 0.8);
	    ***/
            d3.selectAll(sel)
		.classed('selected-observation', true);
	};
    }
    
    // Return to normal the given observation (mouseout)
    //
    function unSelectObs(inDrag) {
	return (d) => {
	    if (inDrag()) { return; }
	    const sel = '#obs-' + d.properties.obsid;
	    /***
		d3.selectAll(sel).transition()
		.duration(transitionTime)
		.style('fill', '#d8b365')
		.style('fill-opacity', 0.4);
	    ***/
	    d3.selectAll(sel)
		.classed('selected-observation', false);
	};
    }

    // Rotate the globe from the current to new position.
    // Animated, if at all possible.
    //
    function rotateFromTo(svg, projection, path, curPos, newPos) {
	// almost-certainly a nicer way to do this
	//
	const interpolator = d3.geoInterpolate(curPos, newPos);
	let frac = 0;
	const dfrac = 0.05;

	// TODO: make dfrac and/or step time a function of
	//       the separation?
	//
	// const sep = d3.geoDistance(curPos, newPos) * to_degrees;

	const timer = d3.timer((e) => {
	    // due to rounding this can mean we don't necessarily scroll
	    // all the way to the new location, hence this final rotation.
	    //
	    if (frac > 1.0) {
		projection.rotate([- newPos[0], - newPos[1]]);
		svg.selectAll('path').attr('d', path);
		timer.stop();
	    }

	    const pos = interpolator(frac);
	    projection.rotate([- pos[0], - pos[1] ]);
	    svg.selectAll('path').attr('d', path);

	    frac += dfrac;
	},
		 50);
    }

    // Create the projection and add it to the #sky element.
    //
    // Note that this creates an id which is hard-coded (at present)
    // so it can not be called multiple times. Although that is to
    // add lighting which I am not sure I want to do.
    //
    // data is a list of observations, which are assumed to be
    // objects with the following keys:
    //     ra     - decimal degrees
    //     dec    - decimal degrees
    //     expks  - exposure time in ks
    //     target - the target name
    //     obisd  - the obsid value
    //
    // The ra0/dec0 values are the RA and Dec (decimal degrees) to
    // start the display at.
    //
    // showObsId is the function to call to change the display to a
    // given ObsId.
    //
    function createSky(showObsId, ra0, dec0, data) {

	// Convert to GeoJSON
	//
	const features = data.map((d) => {
	    return {type: "Feature",
		    geometry: {
			type: "Point",
			// Am I really sure what I'm doing ...
			coordinates: [180 - d.ra, d.dec]
		    },
		    properties: {
			target: d.target,
			obsid: d.obsid,
			exposure: d.expks
		    }
		   };
	});

	// Should we query parentDiv for size rather than hard-coding it?
	//
	// There is a square for the projection, and below that
	// a rectangle which is used for the legend. Rather than
	// have a single SVG contain this, have two of them.
	//
	const svg = d3.select('#sky').append('svg')
	      .attr('width', width)
	      .attr('height', height);

	// Have a "baseplane" for graticules and MW and a "dataplane"
	// for the sources. This lets us fill them in however we
	// want but to control the display ordering.
	//
	const baseplane = svg.append("g").attr('class', 'baseplane');
	const dataplane = svg.append("g").attr('class', 'dataplane');
	
	// Starting location; need to convert from location to
	// angles
	//
	const rotate = [ra0 - 180.0, -dec0, 0];

        const projection = d3.geoOrthographic()
              .scale(width / 2)
              .translate([width / 2, height / 2])
              .clipAngle(90)
              .rotate(rotate)
	;

        const path = d3.geoPath().projection(projection);

	baseplane.append('path')
	    .datum({type: 'Sphere'})
            .attr('class', 'globe')
            .attr('d', path);

        const graticule = d3.geoGraticule()
	      .stepMinor([30, 30])
	      .extentMinor([[-180, -85.01], [180, 85.01]]);

        baseplane.append('path')
	    .datum(graticule())
            .attr('class', 'graticule')
            .attr('d', path);

	addMW(baseplane, path);

	const drag = dragSphere(svg, path, projection)

	// Compute the radius scale based on the exposure time, using
	// square-root so the area scales with t.
	//
	const tscale = d3.scaleSqrt()
            .domain([0, 150])  // not many obs have texp > 150ks so cap here
            .range([3, 10]);

        path.pointRadius((d) => {
	    return d.properties ? tscale(d.properties.exposure) : 1;
	});

	dataplane.selectAll('.observation')
	    .data(features)
            .enter()
	    .append('a')
	    .attr('xlink:href', '#')
	    .attr('data-obsid', (d) => { return d.properties.obsid; })
	    .on('click', (d) => {
		showObsId(d.properties.obsid);

		const r = projection.rotate();
		const curPos = [-r[0], -r[1]];
		const newPos = d.geometry.coordinates;

		rotateFromTo(svg, projection, path, curPos, newPos);
		
	    })
	    .append('path')
            .attr('class', 'observation')
	    .attr('id', (d) => { return "obs-" + d.properties.obsid; })
	    .on('mouseover', selectObs(drag.inDrag))
	    .on('mouseout',  unSelectObs(drag.inDrag))
            .attr('d', path)
	    .append('title')
	    .text((d) => { return d.properties.target; });

        baseplane.call(drag.drag);

	// Add in an indication of the mapping from source size to
	// exposure. Assume that a given radius has the same meaning
	// here as it does on the orthographic projection.
	//
	// Chose "nice" times in hours (but texps is in ks). Note that
	// 150 ks (the cap) is 41 2/3 hours (ie just less than 2 days).
	//
	const lheight = 40;
	const hwidth = width / 2;
	const hheight = lheight / 2;
	
	const lsvg = d3.select('#sky-legend').append('svg')
	      .attr('width', width)
	      .attr('height', lheight);

	lsvg.append('text')
	    .attr('class', 'label')
	    .attr('text-anchor', 'end')
	    .attr('x', hwidth)
	    .attr('y', hheight)
	    .attr('dx', '-2em')
	    .attr('dy', '0.5em')
	    .text('Observation length:');
	
	const texps = [3.6 * 1, 3.6 * 12, 3.6 * 24];
	const tlbls = ["1 hour", "12 hours", "1 day"];

	const xpos = (d, i) => { return hwidth + i * 80; };
	
	lsvg.selectAll('circle.legend-marker')
	    .data(texps)
	    .enter()
	    .append('circle')
	    .attr('class', 'legend-marker')
	    .attr('cx', xpos)
	    .attr('cy', hheight - 7)
	    .attr('r', tscale)
	;
	
	lsvg.selectAll('text.legend-label')
	    .data(tlbls)
	    .enter()
	    .append('text')
	    .attr('class', 'legend-label')
	    .attr('x', xpos)
	    .attr('y', hheight)
	    .attr('dy', '1em')
	    .attr('text-anchor', 'middle')
	    .text((d) => { return d; });

    }

    // Add in the Milky-Way outline. We load it in once and then
    // store it.
    //
    var mwFeatures = null;

    function addMW(plane, path) {

	if (mwFeatures === null) {
	    d3.json("/data/mw-hack.json")
		.then((mw) => {
		    mwFeatures = mw.features;
		    plotMW(plane, path);
		})
		.catch((error) => {
		    console.log("ERROR: unable to load MW data");
		});
	} else {
	    plotMW(plane, path);
	}
    }

    function plotMW(plane, path) {
	plane.selectAll('.milkyway')
	    .data(mwFeatures)
	    .enter()
	    .append("path")
	    .attr("class", "milkyway")
	    .attr("d", path)
	/***
	    .on('mouseover', () => console.log(">> MW"))
	    .on('mouseout', () => console.log("<< MW"))
	    ***/
	    .append("title")
	    .text("Milky Way")
	    ;
    }
    
    return { create: createSky };
    
})();


////// OLD
/***

        // Set the width and height of the SVG container
        var width = 400,
            height = 400;

        // Select the container div and append the SVG element
        var div = d3.select('#map'),
            svg = div.append('svg').attr('width', width).attr('height', height),
            grp = svg.append('g').attr('class', 'gmap');

        // Add a lighting effect to give the circle a spherical aspect
        var filter = svg.append('filter').attr('id', 'lightMe');

        filter.append('feDiffuseLighting')
            .attr('in', 'SourceGraphic')
            .attr('result', 'light')
            .attr('lighting-color', 'white')
            .append('fePointLight')
                .attr('x', 0.85 * width)
                .attr('y', 0.85 * height)
                .attr('z', 50);

        filter.append('feComposite')
            .attr('in', 'SourceGraphic')
            .attr('in2', 'light')
            .attr('operator', 'arithmetic')
            .attr('k1', '1')
            .attr('k2', '0')
            .attr('k3', '0')
            .attr('k4', '0');

        // Projectioon and Path Generator
        // ------------------------------

        // Store the current rotation
        var rotate = {x: 0, y: 90};

        // Create and configure an instance of the orthographic projection
        var projection = d3.geo.orthographic()
            .scale(width / 2)
            .translate([width / 2, height / 2])
            .clipAngle(90)
            .rotate([rotate.x / 2, -rotate.y / 2]);

        // Create and configure the geographic path generator
        var path = d3.geo.path().projection(projection);

        // Overlay
        // -------
        var overlay = svg.selectAll('circle').data([rotate])
            .enter().append('circle')
            .attr('transform', 'translate(' + [width / 2, height / 2] + ')')
            .attr('r', width / 2)
            .attr('filter', 'url(#lightMe)')
            .attr('class', 'overlay');

        // Globe Outline
        // -------------
        var globe = grp.selectAll('path.globe').data([{type: 'Sphere'}])
            .enter().append('path')
            .attr('class', 'globe')
            .attr('d', path);

        // Graticule
        // ---------
        var graticule = d3.geo.graticule();

        // Draw graticule lines
        grp.selectAll('path.graticule').data([graticule()])
            .enter().append('path')
            .attr('class', 'graticule')
            .attr('d', path);

        // Load the stellar catalog
        d3.json('hyg.json', function(error, data) {

            // Handle errors getting and parsing the data
            if (error) { return error; }

            // Compute the radius scale. The radius will be proportional to
            // the aparent magnitude
            var rScale = d3.scale.linear()
                .domain(d3.extent(data.features, function(d) { return d.properties.mag; }))
                .range([3, 1]);

            // Compute the radius for the point features
            path.pointRadius(function(d) {
                return d.properties ? rScale(d.properties.mag) : 1;
            });

            // Stars
            // -----
            grp.selectAll('path.star').data(data.features)
                .enter().append('path')
                .attr('class', 'star')
                .attr('d', path);

            // Drag Behavior
            // -------------
            var dragBehavior = d3.behavior.drag()
                .origin(Object)
                .on('drag', function(d) {
                    projection.rotate([(d.x = d3.event.x) / 2, -(d.y = d3.event.y) / 2]);
                    svg.selectAll('path').attr('d', function(u) {
                        // The circles are not properly generated when the
                        // projection has the clipAngle option set.
                        return path(u) ? path(u) : 'M 10 10';
                    });
                });

            // Add the drag behavior to the overlay
            overlay.call(dragBehavior);
        });

***/
