/*
 * plot up data using the Aitoff projection
 */

var baseOpacity = 0.6;
var unselOpacity = 0.3;
var selOpacity = 0.8;

// This must be < 0.5; at least, once it is used it should be
var baseMWOpacity = 0.3;

// time for a transition, in milliseconds
var transitionTime = 600;
// var mwTransitionTime = 2000;
var mwTransitionTime = 800;

// coords is an array of objects with
// ra/dec attributes in degrees (0-360 and -90 to 90)
// as well as other attributes useful for labelling
// and identifying the targets
//
function createMap(coords) {
  var width = 960;
  var height = 500;

  var tscale = d3.scale.sqrt()
        .domain([0, 150]) // not many obs are larger than this (in ks)
        .range([3, 15]);

  // colors from Cynthia Brewer's http://colorbrewer2.org/ web site
  var color = d3.scale.ordinal()
    .domain(["done", "doing", "todo"])
    // .range([d3.rgb('#ece7f2'), d3.rgb('#a6bddb'), d3.rgb('#2b8cbe')]);
    // .range([d3.rgb('#d8b365'), d3.rgb('#f5f5f5'), d3.rgb('#5ab4ac')]);
    // f5f5f5 is too close to white to be useful here
    .range([d3.rgb('#d8b365'), d3.rgb('#f5f5f5'), d3.rgb('#5ab4ac')]);

  var svg = d3.select('div#map').append('svg')
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

  svg.append("g").attr("id", "baseplane");
  svg.append("g").attr("id", "dataplane");
    
  d3.json("/data/mw-hack.json", function(error, mw) {
      if (error) {
          return console.warn("Unable to load mw-hack.json");
      }
      // Because I have to flip the latitude coordinates,
      // the inside/outside gets all messed up, so shading does
      // not work. This is not needed with mw-hack.json
      /*
      d3.map(mw.features, function(d) {
          cds = d['geometry']['coordinates'];
          for (var i1 = 0; i1 < cds.length; i1++) {
              for (var i2 = 0; i2 < cds[i1].length; i2++) {
                  for (var i3 = 0; i3 < cds[i1][i2].length; i3++) {
                      cds[i1][i2][i3][0] = 180.0 - cds[i1][i2][i3][0];
                  }
                  cds[i1][i2].reverse();
              }
          }
      });
      */

      var oline = svg.select("#baseplane").selectAll(".milkyway")
          .data(mw.features);

      oline.enter()
          .append("path")
          .attr("class", "milkyway")
          .attr("d", path)
          // .attr("opacity", 0)
          .attr("opacity", baseMWOpacity)
          .on('mouseover', function(d) { highlightMW(); })
          .on('mouseout', function(d) { revertMW(); })
          .append("title")
          .text("Milky Way");

      // fade in the outline; not sure I'm 100% happy with this but
      // it seems better than the jaring "just pop in" behavior
      // without it, since it's being added via a callback.
      //
      // I have now switched in to fading in the whole plot once
      // the MW has loaded.
      /*
      oline.transition()
          .duration(mwTransitionTime)
          .attr("opacity", baseMWOpacity);
      */

      svg.transition()
          .duration(mwTransitionTime)
          .attr("opacity", 1);
  });
    
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

  // mark the observations

  var points = coords.map(function (d) {
      // TODO: worry about clipping?
      var pos = projection([d.longitude, d.latitude]);
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
      .text(function(d) { return d.label; });  

}

function highlightMW() {
  d3.selectAll('.milkyway').transition()
        .duration(transitionTime)
        .attr("opacity", 2 * baseMWOpacity);
}

function revertMW() {
  d3.selectAll('.milkyway').transition()
        .duration(transitionTime)
        .attr("opacity", baseMWOpacity);
}

/* Highlight the given object in the sky map */
function selectObs(lbl) {
  d3.select('#' + lbl).classed('selrow', true);
  var idlbl = 'gfx-' + lbl;
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
