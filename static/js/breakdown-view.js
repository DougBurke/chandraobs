/*
 * Based on the code presented in http://bl.ocks.org/mbostock/1166403
 * after significant modification. All bugs are mine.
 */

"use strict";

var createBreakdown = (function () {
    
var margin = {top: 30, right: 20, bottom: 40, left: 80},
    width = 960 - margin.left - margin.right,
    height = 160 - margin.top - margin.bottom;

var x = d3.time.scale()
    .range([0, width]);

var y = d3.scale.linear()
    .range([height, 0]);

var xAxis = d3.svg.axis()
    .scale(x)
// .tickSize(-height)
    .ticks(6);

var yAxis = d3.svg.axis()
    .scale(y)
    .ticks(6)
    // .tickFormat(function(d) { return d + "h"; }) // how to make a superscript?
    .orient("left");

var svg;

function createBreakdown(seriesData) {

    var totHeight = height + margin.top + margin.bottom;
    d3.select("#seriesBlock").style("height", totHeight);

    /*
     * For now just display the year 2015. Note that the input
     * arrays are sparse, so empty days need to be filled in
     * or else the plots can give the wrong impression.
     *
     * The dates are in UTC, so need to work in UTC
     */
    var startDate = new Date("2015-01-01");
    var endDate = new Date("2016-01-01");

    var series = seriesData.series.filter(function (d) {
        var date = new Date(d.date);
        return (date >= startDate) && (date < endDate);
    });
    var seriesMap = series.reduce(function(o, d) {
        var date = new Date(d.date);
        o[date] = d.values;
        return o;
    }, {});
    
    var plotData = d3.time.day.utc.range(startDate, endDate).map(function(d) {
        var values = seriesMap[d] || {};
        return { "date": d, "values": values };
    });
    
    // convert from ks to hours; the results may be surprising,
    // in that there are apparently days with ~40h in them, but
    // that is because the exposure time is associated with the
    // start day *only*.
    var tnorm = 1000.0 / 3600.0;
    
    /*
     * Create an array of series, one for each label.
     * Is there a neater way to do this?
     */

    var getData = function(i) {
        return plotData.map(function(d) {
            var keys = d3.keys(d.values).filter(function(k) {
                return k.startsWith(labels[i]);
            });
            var ys = keys.reduce(function(s, k) {
                return s + d.values[k];
            }, 0);
            return { x: d.date,
                     y0: 0,
                     y: ys * tnorm
                   };
        });
    };
    
    // var labels = ["ACIS-I+NONE", "ACIS-S+NONE"];
    var labels = ["ACIS-I", "ACIS-S", "HRC-I", "HRC-S"];
    var toplot = new Array(labels.length);
    for (var i = 0; i < labels.length; i++) {
        toplot[i] = {
            'label': labels[i],
            'data': getData(i)
        };
    }
    
    // Update the scale domains.
    x.domain([startDate, endDate]);
    y.domain([0, d3.max(toplot, function(a) { return d3.max(a.data, function(d) { return d.y; }); })]);

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
    var axgap = 10;
    var xax = svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + (height+axgap) + ")")
        .call(xAxis);
    
    var yax = svg.append("g")
        .attr("class", "y axis")
        .attr("transform", "translate(" + (-axgap) + ",0)")
        .call(yAxis);

    yax.append("text")
        .attr("x", -(height/2))
        .attr("y", "-2.2em")
        .attr("text-anchor", "middle")
        .attr("transform", "rotate(270 0 0)")
        .text("Hours");
    
    var line = d3.svg.line()
        .interpolate("linear")
        .x(function(d) { return x(d.x); })
        .y(function(d) { return y(d.y); });

    svg.selectAll(".timeline")
        .data(function(d) { return [d.data]; })
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
        .text(function(d) { return d.label; });
}

    return createBreakdown;
})();
