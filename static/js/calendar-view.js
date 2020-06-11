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

    var rect;
    const color = d3.scaleQuantize()
            .domain([0, 8])
            .range(d3.range(9).map(d => `q${d}-9`));

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
      /***
	color = d3.scaleQuantize()
            .domain([0, 8])
            .range(d3.range(9).map(d => `q${d}-9`));
      ***/
      
	addColorbar(0);

	// What order do we want the years displayed in (increasing or
	// decreasing)?
	//
	// var years = d3.range(startYear, endYear + 1);
	const years = d3.range(endYear, startYear - 1, -1);

      // vertical spacing, in pixels, between the graphs
      const spacing = 4;
      
      const svg = d3.select("div#calendar")
            .append("svg")
	      .attr('id', 'calendars')
	    .attr("width", width)
            .attr("height", years.length * height + (years.length - 1) * spacing)
	    .selectAll(".year")
            .data(years)
            .enter().append("g")
            .attr("id", d => `year-${d}`)
            .attr("transform", (d, i) => {
	      const dx = (width - cellSize * 53) / 2;
	      const dy = height - cellSize * 7 - 1 + i * (height + spacing);
	      return `translate(${dx},${dy})`;
	    }
	    );
		  

	// year label
	svg.append("text")
            .attr("transform", "translate(-6," + cellSize * 3.5 + ")rotate(-90)")
            .style("text-anchor", "middle")
            .text(d => d);

	// draw each day
	//
	// TODO: instead of datum(format), perhaps should create an object with
	// all the information needed to create labels and links.
	//
	rect = svg.selectAll(".day")
            .data((d) => {
		const days = d3.timeDays(new Date(d, 0, 1),
					 new Date(d + 1, 0, 1));
		return days.filter(d => (d >= startDate) && (d <= endDate));
            })
            .enter()
            .append("a")
            .attr("xlink:href", d => "/schedule/date/" + format(d) + "/3")
            .append("rect")
            .attr("class", "day")
            .attr("width", cellSize)
            .attr("height", cellSize)
            .attr("x", d => weekOfYear(d) * cellSize)
            .attr("y", d => d.getDay() * cellSize)
            .datum(format);

	// label with the date
	rect.append("title")
            .text(d => d + ": no data");

	const monthFilter = (d) => {
            const months = d3.timeMonths(new Date(d, 0, 1),
					 new Date(d + 1, 0, 1));
            return months.filter(d => (d >= startMonth) && (d < endMonth));
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
            .text(d => monthName[d.getMonth()]);

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

      /***
      const calDiv = document.getElementById('calendar');
      let p = document.createElement('p');
      calDiv.appendChild(p);

      const calSVG = document.getElementById('calendars');
	
      const link = document.createElement('a');
      link.innerHTML = 'To SVG';

      link.addEventListener('click', () => {
	const win = window.open();
	win.document.write(`<iframe src='${svgToLink(calSVG)}' frameborder='0' style='border: 0; bottom: 0; left: 0; right: 0; top: 0; height; 100%; width: 100%;' allowfullscreeen></iframe>`); 
      });

      p.appendChild(link);
      ***/
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
              .attr("transform",
		    `translate(0,${padding})`);
    
	// right-align the box
	const xpos = width - 10 * cellSize * 2;

	cbar.selectAll("rect")
            .data(dataset)
            .enter()
            .append("rect")
            .attr("width", cellSize * 2)
            .attr("height", cellSize * 2)
            .attr("x", d => xpos + d * cellSize * 2)
            .attr("y", 0)
            .attr("class", d => "day " + color(d));

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
            .attr("x", d => xpos + d * cellSize * 2)
            .attr("y", cellSize * 2)
            .attr("dx", "1em")
            .attr("dy", "1em")
            .attr("text-anchor", "middle")
            .text((d) => { if (d < 8) { return String(d); }
			   else { return "8+"; } });
    }


    const transitionTime = 600;

    function highlightDay(dd) {
	rect.filter(d => d === dd)
            .transition()
            .duration(transitionTime)
            .attr("opacity", opacitySel);
    }

    function unhighlightDay(dd) {
	rect.filter(d => d === dd)
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

  // HACK
//
// See
// https://stackoverflow.com/questions/23218174/how-do-i-save-export-an-svg-file-after-creating-an-svg-with-d3-js-ie-safari-an
// http://bl.ocks.org/Rokotyan/0556f8facbaf344507cdc45dc3622177
//
// for the code and inspiration used in this file
//

// Given a document node (assumed to be a SVG item) do something.
//
function svgToLink(svg) {
  const node = svg.cloneNode(true);
  // const node = svg.node();
  node.setAttribute('xlink', 'http://www.w3.org/1999/xlink');

  const styles = getStyles(node);
  addStyles(styles, node);

  const serializer = new XMLSerializer();
  let out = serializer.serializeToString(node);
  
  // Fix root xlink without namespace
  out = out.replace(/(\w+)?:?xlink=/g, 'xmlns:xlink=');
  
  // Safari NS namespace fix
  out = out.replace(/NS\d+:href/g, 'xlink:href');

  /***

This is from similar code

  //add name spaces.
if(!source.match(/^<svg[^>]+xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)){
    source = source.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
}
if(!source.match(/^<svg[^>]+"http\:\/\/www\.w3\.org\/1999\/xlink"/)){
    source = source.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
}

//add xml declaration
source = '<?xml version="1.0" standalone="no"?>\r\n' + source;
***/

  out = '<?xml version="1.0" standalone="no"?>\n' + out;
  return "data:image/svg+xml;charset=utf-8," + encodeURIComponent(out);
}

function contains(str, arr) {
  return arr.indexOf(str) !== -1;
}

function getStyles(element) {
  const selectors = [];
  
  // Add Parent element Id and Classes to the list
  selectors.push('#' + element.id);
  for (let className of element.classList) {
    const selector = '.' + className;
    if (!contains(selector, selectors)) {
      selectors.push(selector);
    }
  }
  
  // Add Children element Ids and Classes to the list
  const nodes = element.getElementsByTagName("*");
  for (let node of nodes) {
    const selector = '#' + node.id;
    if (!contains(selector, selectors)) {
      selectors.push(selector);
    }

    for (let className of node.classList) {
      const selector2 = '.' + className;
      if (!contains(selector2, selectors)) {
	selectors.push(selector2);
      }
    }
  }
    
  // Extract CSS Rules
  var out = "";
  for (let sheet of document.styleSheets) {
    try {
      if (!sheet.cssRules) { continue; }
    } catch (e) {
      if (e.name !== 'SecurityError') { throw e; }
      continue;
    }

    for (let rule of sheet.cssRules) {
      if (!contains(rule.selectorText, selectors)) { continue; }
      out += rule.cssText;
    }
  }

  console.log(`styles=\n${out}`);
  
  return out;
}


// Add the styles (given as a string) to the element.
//
function addStyles(cssText, element) {
  const style = document.createElement("style");
  style.setAttribute("type","text/css"); 
  style.innerHTML = cssText;
  
  const refNode = element.hasChildNodes() ? element.children[0] : null;
  element.insertBefore(style, refNode);
}

  
  // END HACK
  
    return createCalendar;
    
})();
