//
// Based on https://bl.ocks.org/mbostock/4063318
//

var width = 960,
    height = 136,
    cellSize = 17; // cell size

var format = d3.time.format("%Y-%m-%d");

var color;

var svg;
var rect;

// opacity for the cells
/*
var opacityRest = 0.8;
var opacitySel = 1.0;
var opacityUnSel = 0.5;
*/
var opacityRest = 1.0;
var opacitySel = 0.8;

function createCalendar(cal) {

    var startDate = new Date(cal.startDate);
    var endDate = new Date(cal.endDate);

    var startYear = startDate.getUTCFullYear();
    var endYear = endDate.getUTCFullYear();

    var counts = cal.counts;
    var minCount = 1;
    var maxCount = d3.max(d3.values(counts)) || minCount;

    /*
    // use a non-linear scaling; log squashes things too much
    // I think I prefer a more cube-helix (or GitHub) like scheme
    var domain = [minCount, maxCount].map(Math.sqrt);
    color = d3.scale.quantize()
        .domain(domain)
        .range(d3.range(11).map(function(d) { return "q" + d + "-11"; }));
    */

    /*
    var domain = [minCount, maxCount];
    color = d3.scale.quantize()
        .domain(domain)
        .range(d3.range(10).map(function(d) { return "q" + d + "-10"; }));
    */

    var domain = [1, 9];
    color = d3.scale.quantize()
        .domain(domain)
        .range(d3.range(9).map(function(d) { return "q" + d + "-9"; }));

    addColorbar(0);
    
    svg = d3.select("div#calendar").selectAll(".year")
        .data(d3.range(startYear, endYear + 1))
        .enter().append("svg")
        .attr("class", "year")
        .attr("width", width)
        .attr("height", height)
        // .attr("class", "RdYlGn")
        // .attr("class", "d3cat10")
        .append("g")
        .attr("transform",
              "translate(" + ((width - cellSize * 53) / 2) + "," + (height - cellSize * 7 - 1) + ")");

    // year label
    svg.append("text")
        .attr("transform", "translate(-6," + cellSize * 3.5 + ")rotate(-90)")
        .style("text-anchor", "middle")
        .text(function(d) { return d; });

    // draw each day
    rect = svg.selectAll(".day")
        .data(function(d) { return d3.time.days(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
        .enter().append("rect")
        .attr("class", "day")
        .attr("width", cellSize)
        .attr("height", cellSize)
        .attr("x", function(d) { return d3.time.weekOfYear(d) * cellSize; })
        .attr("y", function(d) { return d.getDay() * cellSize; })
        .datum(format);

    // label with the date
    rect.append("title")
        .text(function(d) { return d; });

    // month boundaries
    svg.selectAll(".month")
        .data(function(d) { return d3.time.months(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
        .enter().append("path")
        .attr("class", "month")
        .attr("d", monthPath);

    // add counts info for those days that have it
    rect.filter(function(d) { return d in counts; })
        .attr("class", function(d) { return "day " + color(counts[d]); })
        .attr("opacity", opacityRest)
        .on('mouseover', highlightDay)
        .on('mouseout', unhighlightDay)
        .select("title")
        .text(function(d) {
            var n = counts[d];
            var lbl = d + ": " + counts[d] + " observation";
            if (n > 1) { lbl += "s"; }
            return lbl;
        });

    addColorbar(15);
    
}

/*
 * Add a SVG block displaying the color bar used to color the boxes.
 */

function addColorbar(padding) {
    
    // add a color bar to the display
    var dataset = d3.range(0, 9)

    var cbar = d3.select("div#calendar")
        .append("svg")
        .attr("class", "colorbar")
        .attr("width", width) // use the same size as the year displays
        .attr("height", cellSize * 2 + padding)
        .append("g")
        .attr("transform", "translate(0," + padding + ")");
    
    // right-align the box
    var xpos = width - 10 * cellSize * 2;
    cbar.selectAll("rect")
        .data(dataset)
        .enter()
        .append("rect")
        .attr("width", cellSize * 2)
        .attr("height", cellSize * 2)
        .attr("x", function(d) { return xpos + d * cellSize * 2; })
        .attr("y", 0)
        .attr("class", function(d) { return "day " + color(d); });

    cbar.append("text")
        .attr("class", "description")
        .attr("x", xpos) 
        .attr("y", cellSize)
        .attr("dx", "-1em")
        .attr("dy", "0.5em")
        .attr("text-anchor", "end")
        .text("Number of observations started in a day:");

    cbar.selectAll(".label")
        .data(dataset)
        .enter()
        .append("text")
        .attr("class", "label")
        .attr("x", function(d) { return xpos + d * cellSize * 2; })
        .attr("y", cellSize)
        .attr("dx", "1em")
        .attr("dy", "0.5em")
        .attr("text-anchor", "middle")
        .text(function(d) { if (d < 8) { return String(d+1); } else { return "9+"; } });
}


var transitionTime = 600;

function highlightDay(dd) {
    rect.filter(function(d) { return d === dd; })
        .transition()
        .duration(transitionTime)
        .attr("opacity", opacitySel);
}

function unhighlightDay(dd) {
    rect.filter(function(d) { return d === dd; })
        .transition()
        .duration(transitionTime)
        .attr("opacity", opacityRest);
}

function monthPath(t0) {
  var t1 = new Date(t0.getFullYear(), t0.getMonth() + 1, 0),
      d0 = t0.getDay(), w0 = d3.time.weekOfYear(t0),
      d1 = t1.getDay(), w1 = d3.time.weekOfYear(t1);
  return "M" + (w0 + 1) * cellSize + "," + d0 * cellSize
      + "H" + w0 * cellSize + "V" + 7 * cellSize
      + "H" + w1 * cellSize + "V" + (d1 + 1) * cellSize
      + "H" + (w1 + 1) * cellSize + "V" + 0
      + "H" + (w0 + 1) * cellSize + "Z";
}

