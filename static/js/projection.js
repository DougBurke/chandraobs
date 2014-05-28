/*
 * plot up data using the Aitoff projection
 */

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
     .attr("height", height);

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

  svg.append("path")
    .datum(graticule)
    .attr("class", "graticule")
    .attr("d", path);

  // label graticules; could make it adaptive but hard code
  var longvals = d3.range(1, 6).map(function (d) { 
      var pos = projection([180 - d * 60, 0]);
      var lbl = d * 4 + "h"; // how to get a superscript?
      return { x: pos[0], y: pos[1], lbl: lbl };
    });
  svg.selectAll(".long")
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
      var lbl = d + "\u00B0"; // how to get a degrees character?
      return { x: pos[0], y: pos[1], lbl: lbl };
    });
  svg.selectAll(".lat")
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
  // TODO: highlight the table row when an observation is hovered over

  var points = coords.map(function (d) {
      // TODO: worry about clipping?
      var pos = projection([d.longitude, d.latitude]);
      d.x = pos[0];
      d.y = pos[1];
      return d;
    });

  svg.selectAll(".obs")
      .data(points)
    .enter().append("circle")
      .attr("class", "obs")
      .attr("id", function(d) { return "gfx-" + d.idname; })
      .attr("r", function(d) { return tscale(d.texp); })
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; })
      .attr("opacity", 0.6)
      .style("fill", function(d) { return color(d.status); })
    .on('mouseover', function(d) {
        d3.select('#' + d.idname).classed('selrow', true);
      })
    .on('mouseout', function(d) {
        d3.select('#' + d.idname).classed('selrow', false);
      })
    .append("title")
      .text(function(d) { return d.label; });  

}
