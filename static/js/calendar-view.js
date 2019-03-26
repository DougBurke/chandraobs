"use strict";

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

const createCalendar = (function () {
    
    const width = 960,
	  height = 136,
	  cellSize = 17; // cell size

    const format = d3.timeFormat("%Y-%m-%d");

    var color;

    var svg;
    var rect;

    // replacement for d3.time.weekOfYear from d3 v3
    //
    function weekOfYear(d) {
	return d3.timeWeek.count(d3.timeYear(d), d);
    }
    
    // opacity for the cells; for now have a subtle transition to a
    // slightly-faded version of the cell on mouse-over. This is not
    // ideal (i.e. is likely surprising), but other approaches I tried
    // (fading everything else out or increasing the boundary thickness
    // of the cell) did not appear that useful either.
    // 
    const opacityRest = 1.0;
    const opacitySel = 0.8;

    const monthName = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

    function createCalendar(cal) {

	const startDate = new Date(cal.startDate);
	const endDate = new Date(cal.endDate);

	// add a day onto the end date, as that (hopefully) fixes some issues
	// seen below (maybe there's a timezone conversion going on with some
	// of these dates, which can lead to an apparent issue with dates),
	// so adding on a day is a "hack"
	endDate.setUTCDate(endDate.getUTCDate() + 1);
    
	const startYear = startDate.getUTCFullYear();
	const endYear = endDate.getUTCFullYear();

	const startMonth = new Date(startDate.getUTCFullYear(),
                                    startDate.getUTCMonth(), 1);
	const endMonth = new Date(endDate.getUTCFullYear(),
				  endDate.getUTCMonth() + 1, 1); 

	const counts = cal.counts;
	const minCount = 1;
	const maxCount = d3.max(d3.values(counts)) || minCount;

	// TODO: can now go to a scale starting at 0 counts;
	//       would be nice to go to 9+ rather than 8+
	color = d3.scaleQuantize()
            .domain([0, 8])
            .range(d3.range(9).map((d) => { return "q" + d + "-9"; }));

	addColorbar(0);

	// What order do we want the years displayed in (increasing or
	// decreasing)?
	//
	// var years = d3.range(startYear, endYear + 1);
	const years = d3.range(endYear, startYear - 1, -1);
    
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
            .text((d) => { return d; });

	// draw each day
	//
	// TODO: instead of datum(format), perhaps should create an object with
	// all the information needed to create labels and links.
	//
	rect = svg.selectAll(".day")
            .data((d) => {
		const days = d3.timeDays(new Date(d, 0, 1),
					 new Date(d + 1, 0, 1));
		return days.filter((d) => {
                    return (d >= startDate) && (d <= endDate);
		});
            })
            .enter()
            .append("a")
            .attr("xlink:href", (d) => { return "/schedule/date/" + format(d) + "/3"; })
            .append("rect")
            .attr("class", "day")
            .attr("width", cellSize)
            .attr("height", cellSize)
            .attr("x", (d) => { return weekOfYear(d) * cellSize; })
            .attr("y", (d) => { return d.getDay() * cellSize; })
            .datum(format);

	// label with the date
	rect.append("title")
            .text((d) => { return d + ": no data"; });

	const monthFilter = (d) => {
            const months = d3.timeMonths(new Date(d, 0, 1),
					 new Date(d + 1, 0, 1));
            return months.filter((d) => {
		return (d >= startMonth) && (d < endMonth);
            });
	};

	// month boundaries
	svg.selectAll(".month")
            .data(monthFilter)
            .enter().append("path")
            .attr("class", "month")
            .attr("d", monthPath);

	// month labels
	svg.selectAll(".monthLabel")
            .data(monthFilter)
            .enter().append("text")
            .attr("class", "monthLabel")
            .style("text-anchor", "middle")
            .attr("x", monthX)
	// easiest to place above, as text is cut off if place below the
	// boxes, and I don't want to have to fix that
            .attr("y", 0)
            .attr("dy", "-0.2em")
            .text((d) => { return monthName[d.getMonth()]; });

	// add counts info for those days that have it
	//
	// it is much simpler to have a filter of the form
	//     function(d) { return d in counts; }
	// but I want to include those values with 0 counts,
	// and they are not included in the map. They could
	// be added, but let's try to support sparse data.
	//
	rect.filter((d) => {
            const day = new Date(d);
            return (day >= startDate) && (day <= endDate);
	})
            .attr("class", (d) => {
		const n = counts[d] || 0;
		return "day " + color(n);
            })
            .attr("opacity", opacityRest)
            .on('mouseover', highlightDay)
            .on('mouseout', unhighlightDay)
            .select("title")
            .text((d) => {
		let n = "no";
		if (d in counts) { n = counts[d]; }
		let lbl = d + ": " + n + " observation";
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
	const dataset = d3.range(0, 9);

	const cbar = d3.select("div#calendar")
              .append("svg")
              .attr("class", "colorbar")
              .attr("width", width) // use the same size as the year displays
              .attr("height", cellSize * 3 + padding)
              .append("g")
              .attr("transform", "translate(0," + padding + ")");
    
	// right-align the box
	const xpos = width - 10 * cellSize * 2;

	cbar.selectAll("rect")
            .data(dataset)
            .enter()
            .append("rect")
            .attr("width", cellSize * 2)
            .attr("height", cellSize * 2)
            .attr("x", (d) => { return xpos + d * cellSize * 2; })
            .attr("y", 0)
            .attr("class", (d) => { return "day " + color(d); });

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
            .attr("x", (d) => { return xpos + d * cellSize * 2; })
            .attr("y", cellSize * 2)
            .attr("dx", "1em")
            .attr("dy", "1em")
            .attr("text-anchor", "middle")
            .text((d) => { if (d < 8) { return String(d); }
			   else { return "8+"; } });
    }


    const transitionTime = 600;

    function highlightDay(dd) {
	rect.filter((d) => { return d === dd; })
            .transition()
            .duration(transitionTime)
            .attr("opacity", opacitySel);
    }

    function unhighlightDay(dd) {
	rect.filter((d) => { return d === dd; })
            .transition()
            .duration(transitionTime)
            .attr("opacity", opacityRest);
    }

    // Return the X value corresponding to the last day of the month.
    //
    function monthX(t0) {
	// const m = t0.getMonth();
	const tend = new Date(t0.getFullYear(), t0.getMonth() + 1, 0);
	const wend = weekOfYear(tend);
	return wend * cellSize;
    }

    function monthPath(t0) {
	const t1 = new Date(t0.getFullYear(), t0.getMonth() + 1, 0),
	      d0 = t0.getDay(), w0 = weekOfYear(t0),
	      d1 = t1.getDay(), w1 = weekOfYear(t1);
	return "M" + (w0 + 1) * cellSize + "," + d0 * cellSize
	    + "H" + w0 * cellSize + "V" + 7 * cellSize
	    + "H" + w1 * cellSize + "V" + (d1 + 1) * cellSize
	    + "H" + (w1 + 1) * cellSize + "V" + 0
	    + "H" + (w0 + 1) * cellSize + "Z";
    }

    return createCalendar;
    
})();
