
var totWidth = 960;
var totHeight = 700;

var margin = {top: 10, right: 10, bottom: 60, left: 80 },
    width = totWidth - margin.left - margin.right,
    height = totHeight - margin.top - margin.bottom;

var svg;

var xrange = d3.scale.linear()
    .range([0, width]);
var yrange = d3.scale.linear()
    .range([height, 0]);

var dummy;

function makePlot(plotInfo) {
    dummy = plotInfo;

    /* Plotting up a cumulative function */
    var allInfo = plotInfo['all']
    xrange.domain([allInfo.times[0], allInfo.times[allInfo.length-1]]);
    /* yrange.domain([0, allInfo.length]); */
    yrange.domain([0, 100]);
    
    svg = d3.select("div#exposureplot")
        .append("svg")
        .attr("width", totWidth)
        .attr("height", totHeight)
        .append("g")
        .attr("transform",
              "translate(" + margin.left + "," + margin.top + ")");

    var xAxis = d3.svg.axis()
        .scale(xrange)
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(yrange)
        .orient("left");

    var xax = svg.append("g")
        .attr("class", "x axis")
        .call(xAxis)
        .attr("transform", "translate(0," + height + ")");
    
    xax.append("text")
        .attr("class", "axis")
        .attr("x", width)
        .attr("y", 0)
        .attr("dy", "2.5em")
        .attr("text-anchor", "end")
        .text("Time (ks)");
    
    var yax = svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)

    yax.append("text")
        .attr("class", "axis")
        .attr("x", 0)
        .attr("y", "-3em")
        .attr("text-anchor", "end")
        .attr("transform", "rotate(270 0 0)")
        .text("Fraction of sources with exposure time < value (%)");
    
    /***
    svg.append("clipPath")
      .attr("id", "clip")
      .append("rect")
      .attr("width", width)
      .attr("height", height);
    ***/

    /* Perhaps would be better to create the data in the
     * form needed */
    var line = function(n) {
        return d3.svg.line()
            .interpolate("basis")
            .x(function(d) {
                return xrange(d);
            })
            .y(function(d, i) {
                return yrange(100 * i / n);
            });
    };

    var colors = d3.scale.category10()
        .domain(d3.keys(plotInfo));
    
    for (cycle in plotInfo) {
        svg.append("path")
            .datum(plotInfo[cycle].times)
            .attr("class", "cycle cycle" + cycle)
        /*
            .attr("clip-path", "url(#clip)")
        */
            .style("stroke", colors(cycle))
            .attr("d", line(plotInfo[cycle].length));
    }

}

function createPlot() {
    $.ajax({
        url: '/api/exposures',
    })
        .done(makePlot);
}
