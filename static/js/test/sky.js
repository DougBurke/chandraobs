"use strict";

//
// Create a projection of the sky using D3's orthographic
// projection. Based in part on http://bl.ocks.org/pnavarrc/9730300
// and https://jorin.me/d3-canvas-globe-hover/ (although currently not
// using the latter).
//
// https://d3indepth.com/geographic/ is also useful
//

// Requires that d3 is loaded.
//
const sky = (() => {

    const width = 400;    
    const height = 400;
    const centerPix = [width / 2, height / 2];

    const to_radians = Math.PI / 180;
    const to_degrees = 180 / Math.PI;

    const maxSeparation = Math.PI / 2;

    // Try and improve the drag behavior based on
    // http://bl.ocks.org/ivyywang/7c94cb5a3accd9913263
    // which is from https://www.jasondavies.com/maps/rotate/
    //
    // Perhaps just switch to
    // https://bl.ocks.org/HarryStevens/75b3eb474527c10055618fa00123ba44
    //
    // Other than its responsive-ness, the current implementation can
    // "lose" the anchor point if the user tries to drag too far. This
    // doesn't happen in the Jason Davies/Harry Stevens versions.
    //
    // There is the possibility of using Canvas rather than SVG, which
    // should be more responsive, but makes a lot of other things
    // we do (e.g. mouse over/interactive events) harder.
    //
    // At the moment I use a global variable for the labels as the
    // wiring/ordering is all messed up.
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
		rotateLabels(svg, projection);
	    }
	}

	const dragObj = d3.drag()
	      .on('start', dragStart)
	      .on('drag', drag)
	      .on('end', dragEnd);

	return { drag: dragObj,
		 inDrag: () => posStart !== null
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
		rotateLabels(svg, projection);
		timer.stop();
	    }

	    const pos = interpolator(frac);
	    projection.rotate([- pos[0], - pos[1] ]);
	    svg.selectAll('path').attr('d', path);
	    rotateLabels(svg, projection);

	    frac += dfrac;
	},
		 50);
    }

    // Can we label the sphere (a select few RA/Dec points)?
    //
    function addLabels(plane, projection) {

	const xs = [
	    {pos: raProjection.toLonLat(0, 0), label: '0\u1D34'},
	    {pos: raProjection.toLonLat(8 * 15, 0), label: '8\u1D34'},
	    {pos: raProjection.toLonLat(16 * 15, 0), label: '16\u1D34'},
	    {pos: raProjection.toLonLat(0, 60), label: '60\u00B0'},
	    {pos: raProjection.toLonLat(0, -60), label: '-60\u00B0'},
	    {pos: raProjection.toLonLat(12 * 15, 60), label: '60\u00B0'},
	    {pos: raProjection.toLonLat(12 * 15, -60), label: '-60\u00B0'},
	];

	plane.selectAll('text.axislabel')
	    .data(xs)
	    .enter()
	    .append('text')
	    .attr('class', 'axislabel')
	    .attr('text-anchor', 'middle')
	    .attr('dy', '0.3em')
	    .text(d => d.label);

	rotateLabels(plane, projection);
    }

    // The roll of the rotation is used to calculate the angle for the
    // labels, which is an approximation that fails when the poles
    // are viewed directly. What we want is the angle of the line of
    // constant latitude or longitude at the position.
    //
    function rotateLabels(svg, projection) {
	const angle = projection.rotate()[2];

	svg.selectAll('text.axislabel')
	    .attr('x', (d) => {
		d.proj = projection(d.pos);
		return d.proj[0];
	    })
	    .attr('y', d => d.proj[1])
	    .attr('transform',
		  d => `rotate(${angle},${d.proj[0]},${d.proj[1]})`)
	    .attr('opacity', (d) => {
		// If we're more than PI/2 radians from the center
		// then we're off-screen.
		//
		const pos0 = projection.invert(centerPix);
		const sep = d3.geoDistance(pos0, d.pos);
		return sep > maxSeparation ? 0.0 : 1.0;
	    });
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
			coordinates: raProjection.toLonLat(d.ra, d.dec)
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
	const pos0 = raProjection.toLonLat(ra0, dec0);
	const rotate = [-pos0[0], -pos0[1], 0.0];

	const projection =
	      raProjection.invertXProjection(d3.geoOrthographicRaw)
              .scale(width / 2)
              .translate(centerPix)
              .clipAngle(90)
              .rotate(rotate);
	
        const path = d3.geoPath().projection(projection);
	
	baseplane.append('path')
	    .datum({type: 'Sphere'})
            .attr('class', 'globe')
            .attr('d', path);

        const graticule = d3.geoGraticule()
	      .stepMinor([30, 30])
	      .extentMinor([[-180, -85.01], [180, 85.01]]);

	/***
        baseplane.append('path')
	    .datum(graticule())
            .attr('class', 'graticule')
            .attr('d', path);
	***/

	addMW(baseplane, path);

	// Compute the radius scale based on the exposure time, using
	// square-root so the area scales with t.
	//
	const tscale = d3.scaleSqrt()
            .domain([0, 150])  // not many obs have texp > 150ks so cap here
            .range([3, 10]);

        path.pointRadius(d => d.properties ? tscale(d.properties.exposure) : 1);

	const drag = dragSphere(svg, path, projection)

	dataplane.selectAll('.observation')
	    .data(features)
            .enter()
	    .append('a')
	    .attr('xlink:href', '#')
	    .attr('data-obsid', d => d.properties.obsid)
	    .on('click', (d) => {
		showObsId(d.properties.obsid);

		const r = projection.rotate();
		const curPos = [-r[0], -r[1]];
		const newPos = d.geometry.coordinates;

		rotateFromTo(svg, projection, path, curPos, newPos);
		
	    })
	    .append('path')
            .attr('class', 'observation')
	    .attr('id', d => "obs-" + d.properties.obsid)
	    .on('mouseover', selectObs(drag.inDrag))
	    .on('mouseout',  unSelectObs(drag.inDrag))
            .attr('d', path)
	    .append('title')
	    .text(d => d.properties.target);

	// Does it look better with the graticules on top of the data,
	// for crowded views?
        dataplane.append('path')
	    .datum(graticule())
            .attr('class', 'graticule')
            .attr('d', path);

	addLabels(dataplane, projection);

	// Should the drag be applied to the base plane or the data plane?
	// If it is the base plane then it can be hard to drag when there
	// are so many observations that you can't "grab" any sky.
	// However, switching isn't as simple as just swapping these
	// two lines.
	//
        // dataplane.call(drag.drag);
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
	    .text(d => d);

    }

    // Add in the Milky-Way outline. We load it in once and then
    // store it.
    //
    var mwFeatures = null;

    function addMW(plane, path) {

	if (mwFeatures === null) {
	    d3.json("/data/mw.json")
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

    function hideMW() {
        d3.selectAll('.milkyway')
          .attr("display", "none");
    }

    function showMW() {
        d3.selectAll('.milkyway')
          .attr("display", "inline");
    }

  return { create: createSky, hideMW: hideMW, showMW: showMW };
    
})();
