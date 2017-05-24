
var createPlot = (function () {
    
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

function toHours(ks) { return ks / 3.6; }

function makeLabel(plotInfo, cycle) {
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
    return lbl;
}

function makePlot(plotInfo) {
    dummy = plotInfo;

    var allInfo = plotInfo.all;

    /* Since the X axis (time) is plotted using a log scale,
       use the first non-zero value (ideally all the times
       would be > 0, but some are 0).
       */
    var zeros = allInfo.times.filter(function(d) { return d <= 0; });
    
    /* Plotting up a cumulative function */
    var tmin = allInfo.times[zeros.length];
    var tmax = allInfo.times[allInfo.length - 1];

    /* actually, let's use 0 as the base. */
    /* tmin = 0;  not with a log scale */

    /*
     * X axis is plotted in ks, but the axis is labelled in 
     * hours. For this use case, it is not worth switching
     * to d3.time.scale for xrangeHours, especialy as I use
     * a log scale.
     */
    xrange.domain([tmin, tmax]);
    yrange.domain([0, 100]);

    var xrangeHours = d3.scale.log()
        .range(xrange.range())
        .domain([toHours(tmin), toHours(tmax)]);

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
      .call(yAxis);

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
                return yrange(100 * (i + 1) / n);
            });
    };

    var colors = d3.scale.category10()
        .domain(d3.keys(plotInfo));
    
    for (var cycle in plotInfo) {
        var lbl;
        if (cycle === "all") {
            lbl = "All cycles";
        } else {
            lbl = "Cycle " + cycle;
        }

        plotInfo[cycle].times = plotInfo[cycle].times.
            filter(function (d) { return d > 0; });
        
        svg.append("path")
            .datum(plotInfo[cycle].times)
            .attr("class", "cycle cycle" + cycle)
        /*
            .attr("clip-path", "url(#clip)")
        */
            .style("stroke", colors(cycle))
            .attr("title", lbl)
            .attr("d", line(plotInfo[cycle].length))
            .on("mouseover", highlightCycle(plotInfo, cycle))
            .on("mouseout", removeHighlightCycle(plotInfo, cycle))
        ;

    }

    svg.selectAll(".legend")
        .data(d3.keys(plotInfo))
        .enter()
        .append("text")
        .attr("x", "2em")
        .attr("y", function(d, i) { return (i*1.5) + "em"; })
        .attr("dy", "2em")
        .attr("class", function(cycle) {
            return "legend legend" + cycle;
        })
        .style("fill", colors)
        .text(function(cycle) {
            return makeLabel(plotInfo, cycle);
        });
    
}

function highlightCycle(plotInfo, cycle) {
    return function () {
        d3.selectAll('text.legend' + cycle)
            .text(makeLabel(plotInfo, cycle) + " \u21E6")
            .classed('item-selected', true);
    };
}

function removeHighlightCycle(plotInfo, cycle) {
    return function () {
        d3.selectAll('text.legend' + cycle)
            .text(makeLabel(plotInfo, cycle))
            .classed('item-selected', false);
    };
}

// See https://bl.ocks.org/mbostock/4061502
//

var totBoxWidth = 120;
var totBoxHeight = 500;

var boxMargin = {top: 10, right: 50, bottom: 40, left: 50 },
    boxWidth = totBoxWidth - boxMargin.left - boxMargin.right,
    boxHeight = totBoxHeight - boxMargin.top - boxMargin.bottom;

// Returns a function to compute the interquartile range.
function iqr(k) {
  return function(d, i) {
    var q1 = d.quartiles[0],
        q3 = d.quartiles[2],
        iqr = (q3 - q1) * k,
        i = -1,
        j = d.length;
    while (d[++i] < q1 - iqr);
    while (d[--j] > q3 + iqr);
    return [i, j];
  };
}

var boxChart, boxSvg;

function makeBoxPlot(plotInfo) {

    boxChart = d3.box()
        .whiskers(iqr(1.5))
        .width(boxWidth)
        .height(boxHeight);

    // Note: want to display in hours, not ks
    
    var allInfo = plotInfo.all;
    // var minTime = d3.min(allInfo.times);
    var maxTime = d3.max(allInfo.times);

    // At the moment this is in the order we want
    var data = [];
    var cycles = d3.keys(plotInfo);
    for (var cycle in plotInfo) {
        data.push(plotInfo[cycle].times.map(toHours));
    }

    var colors = d3.scale.category10()
        .domain(cycles);

    // boxChart.domain([toHours(minTime), toHours(maxTime)]);
    boxChart.domain([0, toHours(maxTime)]);

    boxSvg = d3.select("div#exposureboxplot").selectAll(".box")
        .data(data)
        .enter().append("svg")
        .attr("class", "box")
        .attr("width", totBoxWidth)
        .attr("height", totBoxHeight)
        .style("stroke", function(d, i) { return colors(cycles[i]); })
        .append("g")
        .attr("transform", "translate(" + boxMargin.left + "," +
              boxMargin.top + ")")
        .call(boxChart);

    // assume, for now, the ordering is correct
    boxSvg.append("text")
        .attr("class", "cycle")
        .attr("x", boxWidth / 2)
        .attr("y", boxHeight)
        .attr("dy", "2em")
        .attr("text-anchor", "middle")
        .text(function(d, i) {
            var lbl = cycles[i];
            if (lbl === "all") {
                return "All";
            } else {
                return "Cycle " + lbl;
            }
        });
}

function createPlot() {
    $.ajax({
        url: '/api/exposures',
    })
        .done(makePlot)
        .done(makeBoxPlot);
}

    return createPlot;

})();
