
var totWidth = 960;
var totHeight = 700;

var margin = {top: 10, right: 10, bottom: 60, left: 80 },
    width = totWidth - margin.left - margin.right,
    height = totHeight - margin.top - margin.bottom;

var svg;

/*
var xrange = d3.scale.linear()
    .range([0, width]);
*/
var xrange = d3.scale.log()
    .range([0, width]);
var yrange = d3.scale.linear()
    .range([height, 0]);

var dummy;

function makePlot(plotInfo) {
    dummy = plotInfo;

    /* Plotting up a cumulative function */
    var allInfo = plotInfo['all']
    var tmin = allInfo.times[0];
    var tmax = allInfo.times[allInfo.length-1];

    /* actually, let's use 0 as the base. */
    /* tmin = 0;  not with a log scale */

    /*
     * X axis is plotted in ks, but the axis is labelled in 
     * hours. For this use case, it is not worth switching
     * to d3.time.scale for xrangeHours.
    */
    xrange.domain([tmin, tmax]);
    yrange.domain([0, 100]);

    var xrangeHours = d3.scale.log()
        .range(xrange.range())
        .domain([tmin/3.6, tmax/3.6]);

    svg = d3.select("div#exposureplot")
        .append("svg")
        .attr("width", totWidth)
        .attr("height", totHeight)
        .append("g")
        .attr("transform",
              "translate(" + margin.left + "," + margin.top + ")");

    var xAxis = d3.svg.axis()
        .scale(xrangeHours)
        .tickFormat(xrangeHours.tickFormat(7, ".1f"))
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
        .text("Time (hours)");
    
    var yax = svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)

    yax.append("text")
        .attr("class", "axis")
        .attr("x", 0)
        .attr("y", "-3em")
        .attr("text-anchor", "end")
        .attr("transform", "rotate(270 0 0)")
        .text("Percentage of sources with exposure time < value");
    
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
        var lbl;
        if (cycle === "all") {
            lbl = "All cycles";
        } else {
            lbl = "Cycle " + cycle;
        }
        lbl += ": ";
        if (plotInfo[cycle].length == 1) {
            lbl += "one observation";
        } else {
            lbl += plotInfo[cycle].length + " observations";
        }
        lbl += ", " + plotInfo[cycle].totalTime;
        svg.append("path")
            .datum(plotInfo[cycle].times)
            .attr("class", "cycle cycle" + cycle)
        /*
            .attr("clip-path", "url(#clip)")
        */
            .style("stroke", colors(cycle))
            .attr("title", lbl)
            .attr("d", line(plotInfo[cycle].length));
    }

}

function createPlot() {
    $.ajax({
        url: '/api/exposures',
    })
        .done(makePlot);
}
