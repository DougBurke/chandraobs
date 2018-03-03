/*
 * plot up data using the Aitoff projection
 */

"use strict";

var projection = (function (base) {

    var baseOpacity = 0.6;
    var unselOpacity = 0.3;
    var selOpacity = 0.8;
    
    // This must be < 0.5; at least, once it is used it should be
    var baseOpacity = 0.3;

    // time for a transition, in milliseconds
    var transitionTime = 600;
    // var frameTransitionTime = 2000;
    var frameTransitionTime = 800;
    
    // coords is an array of objects with
    // longitude/latitude attributes in degrees (0-360 and -90 to 90)
    // as well as other attributes useful for labelling
    // and identifying the targets
    //
    // The conInfo argument is optional; if set it should be an
    // object with shortName and longName fields (case sensitive),
    // that indicates the constellation boundary to draw.
    //
    function createMap(coords, conInfo) {

        base.hide_nojs();
        
        let width = 960;
        let height = 500;

        let tscale = d3.scale.sqrt()
            .domain([0, 150]) // not many obs are larger than this (in ks)
            .range([3, 15]);

        // colors from Cynthia Brewer's http://colorbrewer2.org/ web site
        let color = d3.scale.ordinal()
            .domain(["done", "doing", "todo"])
        // .range([d3.rgb('#ece7f2'), d3.rgb('#a6bddb'), d3.rgb('#2b8cbe')]);
        // .range([d3.rgb('#d8b365'), d3.rgb('#f5f5f5'), d3.rgb('#5ab4ac')]);
        // f5f5f5 is too close to white to be useful here
            .range([d3.rgb('#d8b365'), d3.rgb('#f5f5f5'), d3.rgb('#5ab4ac')]);

        let svg = d3.select('div#map').append('svg')
            .attr("width", width)
            .attr("height", height)
            .attr("opacity", 0);  // opacity is re-set once MW is loaded

        let projection = d3.geo.aitoff()
            .scale(150)
            .translate([width / 2, height / 2])
            .precision(0.1);

        let path = d3.geo.path()
            .projection(projection);

        let graticule = d3.geo.graticule();

        // Longitude every 2 hours, latitude every 15 degrees,
        // and change the latitude mionor extent so that the
        // -75/75 values are terminated by a longitude graticule
        graticule.minorStep([30, 15]);
        graticule.minorExtent([[-180, -75.01], [180, 75.01]]);

        svg.append("defs").append("path")
            .datum({type: "Sphere"})
            .attr("id", "sphere")
            .attr("d", path);

        svg.append("use")
            .attr("class", "stroke")
            .attr("xlink:href", "#sphere");

        svg.append("use")
            .attr("class", "fill")
            .attr("xlink:href", "#sphere");

        // add in the Milky Way outline before anything else, so it's at
        // the back; since the data is loaded via a callback, use 
        // SVG groups: one for the "labelling" (Milky Way, graticules, ...)
        // and one for the data. Note that I could add in a special
        // group just for the MW, so that it is first, and therefore behind
        // all the other labels, but I don't think that it's worth it.
        //
        // Drawing the constellations can lead to an outline around the whole
        // sky, and I haven't taken the time to identify how to remove it.
        //  
        svg.append("g").attr("id", "baseplane");
        svg.append("g").attr("id", "dataplane");

        addConstellation(svg, path, conInfo);
        addMilkyWay(svg, path);

        svg.select("#baseplane").append("path")
            .datum(graticule)
            .attr("class", "graticule")
            .attr("d", path);

        // label graticules; could make it adaptive but hard code
        let longvals = d3.range(1, 6).map(function (d) { 
            const pos = projection([180 - d * 60, 0]);
            const lbl = d * 4 + "\u1D34"; // this is a capital H super script, may not be in all fonts? 
            return { x: pos[0], y: pos[1], lbl: lbl };
        });
        
        svg.select("#baseplane").selectAll(".long")
            .data(longvals)
            .enter().append("text")
            .attr("class", "label long")
            .attr("x", function(d) { return d.x; })
            .attr("y", function(d) { return d.y; })
            .attr("text-anchor", "middle")
            .attr("dy", "1.4em")
            .text(function(d) { return d.lbl; });

        let latvals = [-75, -45, -15, 15, 45, 75].map(function (d) { 
            const pos = projection([0, d]);
            const lbl = d + "\u00B0"; // degree symbol
            return { x: pos[0], y: pos[1], lbl: lbl };
        });
        
        svg.select("#baseplane").selectAll(".lat")
            .data(latvals)
            .enter().append("text")
            .attr("class", "label lat")
            .attr("x", function(d) { return d.x; })
            .attr("y", function(d) { return d.y; })
            .attr("text-anchor", "end")
            .attr("dx", "-0.4em")
            .attr("dy", "0.35em")
            .text(function(d) { return d.lbl; });

        // mark the observations
        
        let points = coords.map(function (d) {
            // TODO: worry about clipping?
            const pos = projection([d.longitude, d.latitude]);
            d.x = pos[0];
            d.y = pos[1];
            return d;
        });

        svg.select("#dataplane").selectAll(".obs")
            .data(points)
            .enter()
            .append("a")
            .attr("xlink:href", function(d) { return d.urifrag; })
            .append("circle")
            .attr("class", "obs")
            .attr("id", function(d) { return "gfx-" + d.idname; })
            .attr("r", function(d) { return tscale(d.texp); })
            .attr("cx", function(d) { return d.x; })
            .attr("cy", function(d) { return d.y; })
            .attr("opacity", baseOpacity)
            .style("fill", function(d) { return color(d.status); })
            .on('mouseover', function(d) { selectObs(d.idname); })
            .on('mouseout', function(d) { deselectObs(d.idname); })
            .append("title")
            .text(function(d) {
                if (!d.simbadType) { return d.label; }
                else { return d.label + " - " + d.simbadType; }
            });  

    }

    function addMilkyWay(svg, path) {
        const fname = "mw-hack.json";
        d3.json("/data/" + fname, function(error, mw) {
            if (error) {
                return console.warn("Unable to load " + fname);
            }

            let oline = svg.select("#baseplane").selectAll(".milkyway")
                .data(mw.features);
            
            oline.enter()
                .append("path")
                .attr("class", "milkyway")
                .attr("d", path)
                .attr("opacity", baseOpacity)
                .on('mouseover', function(d) { highlightSel('.milkyway'); })
                .on('mouseout', function(d) { revertSel('.milkyway'); })
                .append("title")
                .text("Milky Way");

            // Fade in the whole SVG element
            svg.transition()
                .duration(frameTransitionTime)
                .attr("opacity", 1);
        });
        
    }

    // I have removed the display of the full constellation name
    // since there are issues on what is inside/outside the constellation
    // and given that the constellation name is displayed in the title
    // of the page, it is not a big loss.
    //
    function addConstellation (svg, path, conInfo) {
        if (!conInfo) { return; }
        const fname = "constellations.bounds-hack.json";
        d3.json("/data/" + fname, function(error, con) {
            if (error) {
                return console.warn("Unable to load " + fname);
            }
            
            const conName = conInfo.shortName;
            const conFullName = conInfo.longName;
            
            // all constellations
            let allcon = svg.select("#baseplane").selectAll(".constellations")
                .data(con.features);

            allcon.enter()
                .append("path")
                .attr("class", "constellations")
                .attr("d", path);

            // selected constellation
            let features = con.features.filter(function(d) { return d.id === conName; });
            
            let selcon = svg.select("#baseplane").selectAll(".constellation")
                .data(features);
            
            selcon.enter()
                .append("path")
                .attr("class", "constellation")
                .attr("d", path)
            // .append("title")   currently problems with identifying inside/outside
            // .text(conFullName) the constellation, so remove label
            ;

        });

    }

    function highlightSel(sel) {
        d3.selectAll(sel).transition()
            .duration(transitionTime)
            .attr("opacity", 2 * baseOpacity);
    }

    function revertSel(sel) {
        d3.selectAll(sel).transition()
            .duration(transitionTime)
            .attr("opacity", baseOpacity);
    }
    
    /* Highlight the given object in the sky map */
    function selectObs(lbl) {
        d3.select('#' + lbl).classed('selrow', true);
        const idlbl = 'gfx-' + lbl;
        d3.selectAll('.obs').transition()
            .duration(transitionTime)
            .attr("opacity", function () { return (this.id === idlbl) ? selOpacity : unselOpacity; });
    }

    /* Return it to normal */
    function deselectObs(lbl) {
        d3.select('#' + lbl).classed('selrow', false);
        d3.selectAll('.obs').transition()
            .duration(transitionTime)
            .attr("opacity", baseOpacity);
    }

    /*
     * Can I show filled constellations, using a fill color
     * that is mapped to the observing time for the constellation?
     *
     * For now there is going to be a lot of repeated code
     * with createMap().
     */
    
    /***
function showConstellations(con) {

  var width = 960;
  var height = 500;

  // Used in the calendar
  var bluecols = [
      d3.rgb(247,251,255),
      d3.rgb(222,235,247),
      d3.rgb(198,219,239),
      d3.rgb(158,202,225),
      d3.rgb(107,174,214),
      d3.rgb(66,146,198),
      d3.rgb(33,113,181),
      d3.rgb(8,81,156),
      d3.rgb(8,48,107)
  ];

  // is this the best way to map to a color?
  var tmax = d3.max(d3.values(con), function(d) { return d.tks; });
  var tscale = d3.scale.quantize()
      .domain([0, tmax])
      .range(bluecols);

  // QUESTION: do we want to include the Milky Way?
  var svg = d3.select('div#constellationMap').append('svg')
      .attr("width", width)
      .attr("height", height)
      .attr("opacity", 0);  // opacity is re-set once MW is loaded

  var projection = d3.geo.aitoff()
      .scale(150)
      .translate([width / 2, height / 2])
      .precision(.1);

  var path = d3.geo.path()
      .projection(projection);

  var graticule = d3.geo.graticule();

  // Longitude every 2 hours, latitude every 15 degrees,
  // and change the latitude mionor extent so that the
  // -75/75 values are terminated by a longitude graticule
  graticule.minorStep([30, 15]);
  graticule.minorExtent([[-180, -75.01], [180, 75.01]]);

  svg.append("defs").append("path")
    .datum({type: "Sphere"})
    .attr("id", "sphere")
    .attr("d", path);

  svg.append("use")
    .attr("class", "stroke")
    .attr("xlink:href", "#sphere");

  svg.append("use")
    .attr("class", "fill")
    .attr("xlink:href", "#sphere");

  // add in the Milky Way outline before anything else, so it's at
  // the back; since the data is loaded via a callback, use 
  // SVG groups: one for the "labelling" (Milky Way, graticules, ...)
  // and one for the data. Note that I could add in a special
  // group just for the MW, so that it is first, and therefore behind
  // all the other labels, but I don't think that it's worth it.
  //
  // Drawing the constellations can lead to an outline around the whole
  // sky, and I haven't taken the time to identify how to remove it.
  //  
  svg.append("g").attr("id", "baseplane");
  svg.append("g").attr("id", "dataplane");

  addMilkyWay(svg, path);

  svg.select("#baseplane").append("path")
    .datum(graticule)
    .attr("class", "graticule")
    .attr("d", path);

  // label graticules; could make it adaptive but hard code
  var longvals = d3.range(1, 6).map(function (d) { 
      var pos = projection([180 - d * 60, 0]);
      var lbl = d * 4 + "\u1D34"; // this is a capital H super script, may not be in all fonts? 
      return { x: pos[0], y: pos[1], lbl: lbl };
    });
  svg.select("#baseplane").selectAll(".long")
      .data(longvals)
    .enter().append("text")
      .attr("class", "label long")
      .attr("x", function(d) { return d.x; })
      .attr("y", function(d) { return d.y; })
      .attr("text-anchor", "middle")
      .attr("dy", "1.4em")
      .text(function(d) { return d.lbl; });

  var latvals = [-75, -45, -15, 15, 45, 75].map(function (d) { 
      var pos = projection([0, d]);
      var lbl = d + "\u00B0"; // degree symbol
      return { x: pos[0], y: pos[1], lbl: lbl };
    });
  svg.select("#baseplane").selectAll(".lat")
      .data(latvals)
    .enter().append("text")
      .attr("class", "label lat")
      .attr("x", function(d) { return d.x; })
      .attr("y", function(d) { return d.y; })
      .attr("text-anchor", "end")
      .attr("dx", "-0.4em")
      .attr("dy", "0.35em")
      .text(function(d) { return d.lbl; });

  // The following is based on addConstellation
  //
  // addConstellation(svg, path, conInfo);

  var fname = "constellations.bounds-hack.json";
  d3.json("/data/" + fname, function(error, condata) {
      if (error) {
          return console.warn("Unable to load " + fname);
      }

      // all constellations
      // Use a different class name to get different CSS rules
      var allcon = svg.select("#dataplane").selectAll(".timeconstellations")
          .data(condata.features);

      // As expected, the problem is that we have issues over what is
      // the inside or outside of the constellation....
      
      allcon.enter()
          .append("path")
          .attr("class", "timeconstellations")
          .attr("fill", function(d) {
              if (con[d.id]) { return tscale(con[d.id].tks); }
              else { return "white"; }
          })
          .attr("fill-opacity", function(d) {
              if (con[d.id]) { return 0.8; }
              else { return 0; }
          })
          .attr("d", path);

  });
    
}
    ***/

    return {createMap: createMap,
            selectObs: selectObs,
            deselectObs: deselectObs};
    
})(base);
