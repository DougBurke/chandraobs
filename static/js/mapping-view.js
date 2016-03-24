
var margin = {top: 1, right: 1, bottom: 6, left: 1},
    width = 960 - margin.left - margin.right,
    height = 2000 - margin.top - margin.bottom;

var totWidth = width + margin.left + margin.right;
var totHeight = height + margin.top + margin.bottom;
    
var color = d3.scale.category20();

var sankey = d3.sankey()
    .nodeWidth(15)
    .nodePadding(10)
    .size([width, height]);

var path = sankey.link();

var svg, link;

function countLabel(n) {
    var out = n + " observation";
    if (n > 1) { out += "s"; }
    return out;
}

function makePlot(mapInfo) {

  // remove the animation; the transition is rather abrupt!
  $('#mapping').html("");

  sankey
      .nodes(mapInfo.nodes)
      .links(mapInfo.links)
      .layout(32);

  svg = d3.select("div#mapping").append("svg")
    .attr("width", totWidth)
    .attr("height", totHeight)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
  link = svg.append("g").selectAll(".link")
      .data(mapInfo.links)
    .enter().append("path")
      .attr("class", "link")
      .attr("d", path)
      .style("stroke-width", function(d) { return Math.max(1, d.dy); })
      .sort(function(a, b) { return b.dy - a.dy; });

  link.append("title")
        .text(function(d) { return d.source.name + " → " + d.target.name + "\n" + countLabel(d.value); });

  var node = svg.append("g").selectAll(".node")
      .data(mapInfo.nodes)
    .enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; })
    .call(d3.behavior.drag()
      .origin(function(d) { return d; })
      .on("dragstart", function() { this.parentNode.appendChild(this); })
      .on("drag", dragmove));

  node.append("rect")
      .attr("height", function(d) { return d.dy; })
      .attr("width", sankey.nodeWidth())
      .style("fill", function(d) { return d.color = color(d.name.replace(/ .*/, "")); })
      .style("stroke", function(d) { return d3.rgb(d.color).darker(2); })
    .append("title")
        .text(function(d) { return d.name + "\n" + countLabel(d.value); });

    node
        .append("a")
        .attr("xlink:href", function(d) {
            var out;
            if (mapInfo.proposals.indexOf(d.name) > -1) {
                out = "/search/category/" + encodeURIComponent(d.name);
            } else if (d.name in mapInfo.objects) {
                out = "/search/type/" + encodeURIComponent(mapInfo.objects[d.name].SimbadType);
            } else {
                out = "/error/";
            }
            return out; })
      .append("text")
      .attr("x", -6)
      .attr("y", function(d) { return d.dy / 2; })
      .attr("dy", ".35em")
      .attr("text-anchor", "end")
      .attr("transform", null)
      .text(function(d) { return d.name; })
    .filter(function(d) { return d.x < width / 2; })
      .attr("x", 6 + sankey.nodeWidth())
      .attr("text-anchor", "start");


    
}

function dragmove(d) {
    d3.select(this).attr("transform", "translate(" + d.x + "," + (d.y = Math.max(0, Math.min(height - d.dy, d3.event.y))) + ")");
    sankey.relayout();
    link.attr("d", path);
}


// TODO: handle the error case
function createMapping() {

    // add in animation; this was taken from
    // http://ajaxload.info/
    //
    // also, should try and pre-load it
    //
    // TODO: better center the image in the width of the SVG element
    $('#mapping').html('<div class="waiting"><img width=100 height=100 src="/img/loading.gif" alt="Loading"></div>');

    $.ajax({
        url: '/api/mappings',
    })
        .done(makePlot);
}

