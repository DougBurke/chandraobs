"use strict";

// Enhance the subarray overview visualization
//
// Assumes d3 v5 is loaded
//
// Required element with id=plots for SVG
//
// Requires global vars
//    vals valsByStart valsByWidth
// (the plan is to remove un-needed ones). The values are in kilo-seconds.

const subArrayView = (function() {

    const totWidth = 500;
    const totHeight = 300;
    const margin = { top: 10, right: 10, bottom: 50, left: 80 };
    
    const width = totWidth - margin.left - margin.right;
    const height = totHeight - margin.top - margin.bottom;
    
    var svgStart, svgWidth;

    const ids = [0, 1, 2, 3, 4, 5, 6, 7];

    // Flatten the "2D array"
    //
    function sumTimes(vals) {
	const sum = (a, b) => a + b;
	
	return ids.map((id) => {
	    if (typeof vals[id] !== 'undefined') {
		return Object.values(vals[id]).reduce(sum, 0);
	    } else {
		return 0;
	    }
	});
    }

    function xaxis(scale) {
	return d3.axisBottom()
	  .scale(scale)
          .tickFormat((d, i) => `${1 + d * 128}-${(d + 1) * 128}`);
    }
    
    function yaxis(scale) {
	return d3.axisLeft()
	    .scale(scale);
    }

    // classHead is used to define the per "row" rectangles
    // (the index is added to it)
    //
    // The times are in kilo-seconds, but we want to label them with
    // days.
    function makeHist(svg, data, toFlatten, xScale, xLabel, classHead) {

	const flattened = sumTimes(toFlatten);

	const yScale = d3.scaleLinear()
	      .domain([0, d3.max(flattened)])
	      .range([height, 0]);

	const xStart = xaxis(xScale);
	const yStart = yaxis(yScale);

	svg.append('g')
	    .attr('class', 'x axis')
	    .call(xStart)
	    .attr('transform', `translate(0,${height})`)
	;

	svg.append('g')
	    .attr('class', 'y axis')
	    .call(yStart)
	;

        svg.append('text')
          .attr('class', 'x axis-label')
          .attr('transform',
  	        `translate(${width / 2},${height + margin.top + 40})`)
          .style('text-anchor', 'middle')
          .text(xLabel);
      
        svg.append('text')
          .attr('class', 'y axis-label')
          .attr('transform', 'rotate(-90)')
        .attr('x', 0 - (height / 2))
        .attr('y', 0 - margin.left)
        .attr('dy', '1em')
          .style('text-anchor', 'middle')
          .text('Time (days)');
      
	// Doh: different formats for the data...
	const ids = [0, 1, 2, 3, 4, 5, 6, 7];

	// This seems a bit overkill to create the matrix.
	//
	let matrix = [];
	ids.forEach(id => {
	    let row = [];
	    const array = data[id];
	    if (typeof array === 'undefined') {
		ids.forEach(id2 => row.push(0.0));
	    } else {
		ids.forEach(id2 => {
		    const v = array[id2];
		    if (typeof v === 'undefined') { row.push(0.0); }
		    else { row.push(v); }
		});
	    }
	    matrix.push(row);
	});

	const lineAll = d3.line()
	      .x((d, i) => xScale(i))
	      .y(yScale)
	;

	const y0 = yScale(0);
	const dx = xScale(1) - xScale(0);

	svg.selectAll('.all')
	    .data(ids)
	    .enter()
	    .append('rect')
	    .attr('class', 'hist all')
	    .attr('x', d => xScale(d - 0.5))
	    .attr('width', dx)
	    .attr('y', d => yScale(flattened[d]))
	    .attr('height', d => y0 - yScale(flattened[d]));

	// Ugly way to do this
	//
	ids.forEach(id => {
	    const sel = classHead + id;
	    svg.selectAll('.' + sel)
		.data(ids)
		.enter()
		.append('rect')
		.attr('class', 'hist sel ' + sel)
		.attr('x', d => xScale(d - 0.5))
		.attr('width', dx)
		.attr('y', d => yScale(matrix[id][d]))
		.attr('height', d => y0 - yScale(matrix[id][d]));
	});
	
    }
    
    function initPage() {

	svgStart = d3.select('#plots')
	    .append('svg')
	    .attr('id', 'byStart')
	    .attr('width', totWidth)
	    .attr('height', totHeight)
	    .append('g')
	    .attr('transform',
		  `translate(${margin.left},${margin.top})`);

	svgWidth = d3.select('#plots')
	    .append('svg')
	    .attr('id', 'byWidth')
	    .attr('width', totWidth)
	    .attr('height', totHeight)
	    .append('g')
	    .attr('transform',
		  `translate(${margin.left},${margin.top})`);

	const xScale = d3.scaleLinear()
	      .domain([-0.5, 7.5])
	      .range([0, width])
	      // .range(['1/8', '1/4', '3/8', '1/2', '5/8', '3/4', '1'])
	;

	// The trouble with coding by "trying things until they work
	// when you should be concentrating on your kids swim lessons"
	// is that it ends up with code that looks like the following.
	//
        makeHist(svgStart, valsByStart, valsByWidth, xScale,
  	         'Number of rows', 'bins');
        makeHist(svgWidth, valsByWidth, valsByStart, xScale,
	         'Start Row', 'binw');

	// Add the mouse-over for all the cells
	//
	document.querySelectorAll('.cell').forEach(el => {
	    // skip the border elements
	    const s = el.getAttribute('data-start');
	    const w = el.getAttribute('data-width');
	    if ((s === null) || (w === null) ) { return; }

	    el.addEventListener('mouseover', () => {
		svgStart.selectAll('.bin' + s)
		    .style('display', 'inline');
		
		svgWidth.selectAll('.bin' + w)
		    .style('display', 'inline');
	    });
	    el.addEventListener('mouseout', () => {
		svgStart.selectAll('.bin' + s)
		    .style('display', 'none');
		
		svgWidth.selectAll('.bin' + w)
		    .style('display', 'none');
	    });
	});
    }
    
    return { init: initPage };
})();
