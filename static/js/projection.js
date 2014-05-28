/*
 * plot up data using the Mollweide projection
 */

// coords is an array of objects with x/y attributes
// in the range -pi to pi (x) and -pi/2 to pi (y)
//
// TODO: transform to using the d3 geo projection plugin at
//       http://bl.ocks.org/mbostock/3734336
//
function projectionMollweide(coords) {
  // var margin = { top: 20, right: 20, bottom: 20, left: 20 };
  // var width = 600;
  // var height = width / 2;
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

  var projection = d3.geo.mollweide()
      .scale(165)
      .translate([width / 2, height / 2])
      .precision(.1);

  var path = d3.geo.path()
      .projection(projection);

  var graticule = d3.geo.graticule();

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

  // mark the observations
  // TODO: highlight the table row when an observation is hovered over

  var points = coords.map(function (d) {
      // TODO: worry about clipping?
      var pos = projection([d.longitude, d.latitude]);
      d.x = pos[0];
      d.y = pos[1];
      return d;
    });

  // changing the font size is not visually appealing, since it
  // causes a lot of visual changes
  /*
  var selStyle = 'font-size';
  var selOn = '150%';
  var selOff = '100%';
  */
  var selStyle = 'font-weight';
  var selOn = 'bold';
  var selOff = 'normal';

  svg.selectAll(".obs")
      .data(points)
    .enter().append("circle")
      .attr("class", "obs")
      .attr("id", function(d) { return "gfx-" + d.idname; })
      .attr("r", function(d) { return tscale(d.texp); })
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; })
      .attr("opacity", 0.8)
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
