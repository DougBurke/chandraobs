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
  var width = 600;
  var height = width / 2;

  var xscale = d3.scale.linear()
      .domain([-Math.PI, Math.PI])
      .range([0, width]);

  var yscale = d3.scale.linear()
      .domain([-Math.PI/2, Math.PI/2])
      .range([height, 0]);

  var color = function(l) { if (l == 'done') { return d3.rgb(153,153,152); } else if (l == 'todo') { return d3.rgb(153,153,152); } else { return d3.rgb(0, 131, 179); } };

  var tscale = d3.scale.sqrt()
        .domain([0, 150]) // not many obs are larger than this (in ks)
        .range([3, 10]);

  // colors from Cynthia Brewer's http://colorbrewer2.org/ web site
  var color = d3.scale.ordinal()
    .domain(["done", "doing", "todo"])
    .range([d3.rgb('#ece7f2'), d3.rgb('#a6bddb'), d3.rgb('#2b8cbe')]);

  var svg = d3.select('div#map').append('svg')
     .attr("width", width)
     .attr("height", height);

  // a
  svg.lin

  // mark the observations
  // TODO: highlight the table row when an observation is hovered over
  svg.selectAll(".obs")
      .data(coords)
    .enter().append("circle")
      .attr("class", "obs")
      .attr("r", function(d) { return tscale(d.texp); })
      .attr("cx", function(d) { return xscale(d.x); })
      .attr("cy", function(d) { return yscale(d.y); })
      .style("fill", function(d) { return color(d.status); })
    .append("title")
      .text(function(d) { return d.label; });  

}
