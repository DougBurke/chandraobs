//
// Based on https://bl.ocks.org/mbostock/4063318
//
// TODO:
//   there are two sorts of 0-count cells; ideally the
//   0-count cells would be white, those outside the
//   range of interest would be gray. This is possible
//   now.
//
//   Or, only draw cells for which we have data!
//

"use strict";

var createCalendar = (function () {
    
var width = 960,
    height = 136,
    cellSize = 17; // cell size

var format = d3.time.format("%Y-%m-%d");

var color;

var svg;
var rect;

// opacity for the cells; for now have a subtle transition to a
// slightly-faded version of the cell on mouse-over. This is not
// ideal (i.e. is likely surprising), but other approaches I tried
// (fading everything else out or increasing the boundary thickness
// of the cell) did not appear that useful either.
// 
var opacityRest = 1.0;
var opacitySel = 0.8;

var monthName = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

function createCalendar(cal) {

    var startDate = new Date(cal.startDate);
    var endDate = new Date(cal.endDate);

    // add a day onto the end date, as that (hopefully) fixes some issues
    // seen below (maybe there's a timezone conversion going on with some
    // of these dates, which can lead to an apparent issue with dates),
    // so adding on a day is a "hack"
    endDate.setUTCDate(endDate.getUTCDate() + 1);
    
    var startYear = startDate.getUTCFullYear();
    var endYear = endDate.getUTCFullYear();

    var startMonth = new Date(startDate.getUTCFullYear(), startDate.getUTCMonth(), 1);
    var endMonth = new Date(endDate.getUTCFullYear(), endDate.getUTCMonth() + 1, 1); 

    var counts = cal.counts;
    var minCount = 1;
    var maxCount = d3.max(d3.values(counts)) || minCount;

    // TODO: can now go to a scale starting at 0 counts;
    //       would be nice to go to 9+ rather than 8+
    var domain = [0, 8];
    color = d3.scale.quantize()
        .domain(domain)
        .range(d3.range(9).map(function(d) { return "q" + d + "-9"; }));

    addColorbar(0);

    // What order do we want the years displayed in (increasing or
    // decreasing)?
    //
    // var years = d3.range(startYear, endYear + 1);
    var years = d3.range(endYear, startYear - 1, -1);
    
    svg = d3.select("div#calendar").selectAll(".year")
        .data(years)
        .enter().append("svg")
        .attr("class", "year")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform",
              "translate(" + ((width - cellSize * 53) / 2) + "," + (height - cellSize * 7 - 1) + ")");

    // year label
    svg.append("text")
        .attr("transform", "translate(-6," + cellSize * 3.5 + ")rotate(-90)")
        .style("text-anchor", "middle")
        .text(function(d) { return d; });

    // draw each day
    //
    // TODO: instead of datum(format), perhaps should create an object with
    // all the information needed to create labels and links.
    //
    rect = svg.selectAll(".day")
        .data(function(d) {
            var days = d3.time.days(new Date(d, 0, 1), new Date(d + 1, 0, 1));
            return days.filter(function (d) { return (d >= startDate) && (d <= endDate); });
        })
        .enter()
        .append("a")
        .attr("xlink:href", function(d) { return "/schedule/date/" + format(d) + "/3"; })
        .append("rect")
        .attr("class", "day")
        .attr("width", cellSize)
        .attr("height", cellSize)
        .attr("x", function(d) { return d3.time.weekOfYear(d) * cellSize; })
        .attr("y", function(d) { return d.getDay() * cellSize; })
        .datum(format);

    // label with the date
    rect.append("title")
        .text(function(d) { return d + ": no data"; });

    // month boundaries
    svg.selectAll(".month")
        .data(function(d) {
            var months = d3.time.months(new Date(d, 0, 1), new Date(d + 1, 0, 1));
            return months.filter(function (d) { return (d >= startMonth) && (d < endMonth); });
        })
        .enter().append("path")
        .attr("class", "month")
        .attr("d", monthPath);

    // month labels
    svg.selectAll(".monthLabel")
        .data(function(d) {
            var months = d3.time.months(new Date(d, 0, 1), new Date(d + 1, 0, 1));
            return months.filter(function (d) { return (d >= startMonth) && (d < endMonth); });
        })
        .enter().append("text")
        .attr("class", "monthLabel")
        .style("text-anchor", "middle")
        .attr("x", monthX)
    // easiest to place above, as text is cut off if place below the
    // boxes, and I don't want to have to fix that
        .attr("y", 0)
        .attr("dy", "-0.2em")
        .text(function(d) { return monthName[d.getMonth()]; });

    // add counts info for those days that have it
    //
    // it is much simpler to have a filter of the form
    //     function(d) { return d in counts; }
    // but I want to include those values with 0 counts,
    // and they are not included in the map. They could
    // be added, but let's try to support sparse data.
    //
    rect.filter(function(d) {
        var day = new Date(d);
        return (day >= startDate) && (day <= endDate);
    })
        .attr("class", function(d) {
            var n = counts[d] || 0;
            return "day " + color(n);
        })
        .attr("opacity", opacityRest)
        .on('mouseover', highlightDay)
        .on('mouseout', unhighlightDay)
        .select("title")
        .text(function(d) {
            var n = "no";
            if (d in counts) { n = counts[d]; }
            var lbl = d + ": " + n + " observation";
            if (n !== 1) { lbl += "s"; }
            return lbl;
        });
    
    addColorbar(15);
    
}

/*
 * Add a SVG block displaying the color bar used to color the boxes.
 * I tried having the counts in the box, but thought that it was hard
 * to read for the darker cells, so have moved the text below the box.
 * This means that the cells could be drawn at "normal" size, but
 * the labels still need to be large, so leave as is.
 *
 * The padding argument adds vertical space above the display, and is
 * a hack to let it be drawn after the plots (for the first version,
 * call with padding=0).
 */

function addColorbar(padding) {
    
    // add a color bar to the display
    var dataset = d3.range(0, 9);

    var cbar = d3.select("div#calendar")
        .append("svg")
        .attr("class", "colorbar")
        .attr("width", width) // use the same size as the year displays
        .attr("height", cellSize * 3 + padding)
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

    // draw the labels below the cells; unfortunately the sizes
    // are in pixels so this is rather dependent on the font size
    // to display nicely
    cbar.selectAll(".label")
        .data(dataset)
        .enter()
        .append("text")
        .attr("class", "label")
        .attr("x", function(d) { return xpos + d * cellSize * 2; })
        .attr("y", cellSize * 2)
        .attr("dx", "1em")
        .attr("dy", "1em")
        .attr("text-anchor", "middle")
        .text(function(d) { if (d < 8) { return String(d); } else { return "8+"; } });
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

// Return the X value corresponding to the last day of the month.
//
function monthX(t0) {
    var m = t0.getMonth();
    var tend = new Date(t0.getFullYear(), t0.getMonth() + 1, 0);
    var wend = d3.time.weekOfYear(tend);
    return wend * cellSize;
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

    return createCalendar;
    
})();
