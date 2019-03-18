/*
 * Based on the code presented in http://bl.ocks.org/mbostock/1166403
 * after significant modification. All bugs are mine.
 */

"use strict";

const createBreakdown = (function () {
    
    const margin = {top: 30, right: 20, bottom: 40, left: 80},
	  width = 960 - margin.left - margin.right,
	  height = 160 - margin.top - margin.bottom;

    const x = d3.time.scale()
	  .range([0, width]);

    const y = d3.scale.linear()
	.range([height, 0]);

    const xAxis = d3.svg.axis()
	.scale(x)
    // .tickSize(-height)
	.ticks(6);

    const yAxis = d3.svg.axis()
	.scale(y)
	.ticks(6)
    // .tickFormat(function(d) { return d + "h"; }) // how to make a superscript?
	.orient("left");

    var svg;

    function createBreakdown(seriesData) {

	const totHeight = height + margin.top + margin.bottom;
	d3.select("#seriesBlock").style("height", totHeight);

	/*
	 * For now just display the year 2015. Note that the input
	 * arrays are sparse, so empty days need to be filled in
	 * or else the plots can give the wrong impression.
	 *
	 * The dates are in UTC, so need to work in UTC
	 */
	const startDate = new Date("2015-01-01");
	const endDate = new Date("2016-01-01");

	const series = seriesData.series.filter((d) => {
            const date = new Date(d.date);
            return (date >= startDate) && (date < endDate);
	});
	const seriesMap = series.reduce((o, d) => {
            const date = new Date(d.date);
            o[date] = d.values;
            return o;
	}, {});
    
	const plotData = d3.time.day.utc.range(startDate, endDate).map((d) => {
            const values = seriesMap[d] || {};
            return { "date": d, "values": values };
	});
    
	// convert from ks to hours; the results may be surprising,
	// in that there are apparently days with ~40h in them, but
	// that is because the exposure time is associated with the
	// start day *only*.
	const tnorm = 1000.0 / 3600.0;
    
	/*
	 * Create an array of series, one for each label.
	 * Is there a neater way to do this?
	 */

	const getData = function(label) {
            return plotData.map((d) => {
		const keys = d3.keys(d.values).filter((k) => {
                    return k.startsWith(label);
		});
		const ys = keys.reduce((s, k) => {
                    return s + d.values[k];
		}, 0);
		return { x: d.date,
			 y0: 0,
			 y: ys * tnorm
                       };
            });
	};
    
	const labels = ["ACIS-I", "ACIS-S", "HRC-I", "HRC-S"];
	const toplot = [];
	for (var label of labels) {
            toplot.push({
		'label': label,
		'data': getData(label)
            });
	}
    
	// Update the scale domains.
	x.domain([startDate, endDate]);
	y.domain([0,
		  d3.max(toplot,
			 (a) => { return d3.max(a.data, (d) => { return d.y; }); })
		 ]);

	svg = d3.select("#seriesBlock").selectAll(".timeplot")
            .data(toplot)
            .enter()
            .append("svg")
            .attr("class", "timeplot")
            .attr("width", width + margin.left + margin.right)
            .attr("height", totHeight)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	// Set up the axes
	const axgap = 10;
	const xax = svg.append("g")
              .attr("class", "x axis")
              .attr("transform", "translate(0," + (height+axgap) + ")")
              .call(xAxis);

	const yax = svg.append("g")
              .attr("class", "y axis")
              .attr("transform", "translate(" + (-axgap) + ",0)")
              .call(yAxis);

	yax.append("text")
            .attr("x", -(height/2))
            .attr("y", "-2.2em")
            .attr("text-anchor", "middle")
            .attr("transform", "rotate(270 0 0)")
            .text("Hours");

	const line = d3.svg.line()
              .interpolate("linear")
              .x((d) => { return x(d.x); })
              .y((d) => { return y(d.y); });

	svg.selectAll(".timeline")
            .data((d) => { return [d.data]; })
            .enter()
            .append("path")
            .attr("class", "timeline")
            .attr("d", line);

	svg.append("text")
            .attr("class", "title")
            .attr("x", width)
            .attr("y", 0)
            .attr("dx", "-1em")
            .attr("dy", "1.2em")
            .attr("text-anchor", "end")
            .text((d) => { return d.label; });
    }

    return createBreakdown;
})();
