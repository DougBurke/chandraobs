/*
 * Render the SIMBAD types using the hiearchy from
 * http://cds.u-strasbg.fr/cgi-bin/Otype?X
 * based on the d3 example at
 * https://bl.ocks.org/mbostock/4339083
 *
 * The attempted Sankey visualization doesn't look too good;
 * perhaps can take the dendogram and change the link width
 * based on the size of the connection (or a transform
 * of the size - e.g. sqrt or log, but log is probably too
 * severe)
 */

/***
the Json is an object with keys: name -> string, and children -> an array of objects
   or a size field
but I would want to start with an array and allow both size and children
***/

var root;
var duration = 750;

var width0 = 1200;
var height0 = 1000;

var margin = {top: 20, right: 120, bottom: 20, left: 120},
    width = width0 - margin.right - margin.left,
    height = height0 - margin.top - margin.bottom;

var totWidth = width + margin.right + margin.left;
var totHeight = height + margin.top + margin.bottom;

var tree;
var svg;
var diagonal;

// Toggle children on click.
function nodeClick(d) {
    if (d.children) {
        d._children = d.children;
        d.children = null;
    } else {
        d.children = d._children;
        d._children = null;
    }
    update(d);
}

/*
 * TODO: there is a problem with the setting of opacity on nodes, particularly
 *       once some are hidden then re-shown.
 */

// Based on http://bl.ocks.org/mbostock/4062006
function changeOtherNodes(opacity) {
    return function(g, i) {
        // what shall we fade?
        // might it be nice to keep the parent/children?
        svg.selectAll(".node text")
             // TODO: does not select the right nodes!
            .filter(function(d) { return d.id != g.id; })
            .transition()
            .style("opacity", opacity);
    };
}

function update(source) {

    // Compute the new tree layout.
    var nodes = tree.nodes(root).reverse(),
        links = tree.links(nodes);

    // Normalize for fixed-depth.
    nodes.forEach(function(d) { d.y = d.depth * 180; });
    
    // Update the nodes…
    var i = 0;
    var node = svg.selectAll("g.node")
        .data(nodes, function(d) { return d.id || (d.id = ++i); });

    // Unlike the standard d3 dendogram, the text are links, so
    // restrict the expand/hide functionality to just the circle
    
    // Enter any new nodes at the parent's previous position.
    var nodeEnter = node.enter().append("g")
        .attr("class", "node")
        .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
    // QUS: how best to highlight this node + path (parents and/or
    //      descendents? Maybe want the links to be kept too?
        .on("mouseover", changeOtherNodes(0.2))
        .on("mouseout", changeOtherNodes(1))
    ;
    
    nodeEnter.append("circle")
        .attr("r", 1e-6)
        .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; })
        .on("click", nodeClick)
    ;

    // Note that due to previous filtering, all nodes should have a positive
    // size, but leave the check in for future work.
    nodeEnter
        .append("a")
        .attr("xlink:href", function(d) { return d.searchLink; })
        .append("text")
        .attr("x", function(d) { return d.children || d._children ? -10 : 10; })
        .attr("dy", ".35em")
        .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
        .text(function(d) {
            if (d.depth < 1) { return ""; }
            var lbl = d.name;
            // leave the following in for now, even though 0-size elements
            // have been filtered out.
            if (d.size > 0) { lbl += " (" + d.size + ")"; }
            return lbl; })
        .style("fill-opacity", 1e-6)
    ;
    
    // Transition nodes to their new position.
    var nodeUpdate = node.transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });

    nodeUpdate.select("circle")
        .attr("r", 4.5)
        .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

    nodeUpdate.select("text")
        .style("fill-opacity", 1);

    // Transition exiting nodes to the parent's new position.
    var nodeExit = node.exit().transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
        .remove();

    nodeExit.select("circle")
        .attr("r", 1e-6);
    
    nodeExit.select("text")
        .style("fill-opacity", 1e-6);
    
    // Update the links…
    var link = svg.selectAll("path.link")
        .data(links, function(d) { return d.target.id; });
    
    // Enter any new links at the parent's previous position.
    link.enter().insert("path", "g")
        .attr("class", "link")
        .attr("d", function(d) {
            var o = {x: source.x0, y: source.y0};
            return diagonal({source: o, target: o});
        });
    
    // Transition links to their new position.
    link.transition()
        .duration(duration)
        .attr("d", diagonal);
    
    // Transition exiting nodes to the parent's new position.
    link.exit().transition()
        .duration(duration)
        .attr("d", function(d) {
            var o = {x: source.x, y: source.y};
            return diagonal({source: o, target: o});
        })
        .remove();

    // Stash the old positions for transition.
    nodes.forEach(function(d) {
        d.x0 = d.x;
        d.y0 = d.y;
    });
}

var delme;

function createTree(json) {

    delme = json;
    json = json.dendogram;
    
    // filter out all nodes with a size of zero; assume that the
    // top-level element is not empty.
    function removeEmpty(d) {
        if (d.size == 0) { return 0; }
        if (d.children) {
            d.children = d.children.filter(removeEmpty);
        }
        return 1;
    }
    json.children = json.children.filter(removeEmpty);

    svg = d3.select("div#tree").append("svg")
        .attr("class", "dendogram")
        .attr("width", totWidth)
        .attr("height", totHeight)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    tree = d3.layout.tree()
        .size([height, width]);

    diagonal = d3.svg.diagonal()
        .projection(function(d) { return [d.y, d.x]; });
    
    root = json;
    root.x0 = height / 2;
    root.y0 = 0;

    // Collapse all the nodes but the top level;
    // for some reason, calling this messes the display up
    // even though you can collapse/hide the nodes manually.
    function collapseNode(d) {
        if (d.children) {
            d._children = d.children;
            d._children.forEach(collapseNode);
            d.children = null;
        }
    }

    // TODO: do we want the nodes to be collapsed? or even be collapsible?
    // root.children.forEach(collapseNode);
    update(root);

    // TODO: what is this
    // d3.select(self.frameElement).style("height", "800px");
    d3.select("#tree").style("height", height0 + "px");
}



// Sankey visualization
//
// namespace issues!

var color = d3.scale.category20();

var swidth = width;
var sheight = height * 3;

var smargin = margin;

var totSWidth = swidth + smargin.right + smargin.left;
var totSHeight = sheight + smargin.top + smargin.bottom;


// It is possible that d3.sankey isn't loaded by the time this is processed.
// Should probably be in the routine called from onload.
//
var sankey = d3.sankey()
    .nodeWidth(15)
    .nodePadding(10)
    .size([swidth, sheight]);

var path_sankey = sankey.link();

var svg_sankey, link_sankey;

function getNumSrcString(nsrc) {
    var str;
    if (nsrc == 1) {
        str = "One source";
    } else {
        str = nsrc + " sources";
    }
    return str;
}

var dummy;

// Add the node to output and then process the children.
//
// Depth- or breadth-first travesal?
//
// I tried depth-first, but the screen output isn't ideal
// (presumably because it starts off based on the
// ordering), so try depth-first. I do this by adding
// in the depth field, and then sorting on that
// in the final output array.
//
function findNodes(onode, output) {
    // drop the "all" node
    if (onode.depth > 0) {
        var out = {name: onode.name, depth: onode.depth};
        if (onode.searchLink) {
            out['searchLink'] = onode.searchLink;
            out['shortName'] = onode.shortName;
        }
        output.push(out);
    }
    if (onode.children) {
        onode.children.forEach(function(d) { findNodes(d, output); })
    }
}

// Given a node, add it to the output and then
// process the children.
//
// pnum is the parent number (i.e. the index, in
// nodes, for the parent of this node).
//
function findLinks(onode, pnum, nodes, output) {
    // Have flip-flopped over the ordering of this;
    // am now back to not wanting to display the "all -> ?"
    // links, which makes the following messy,
    // as I don't want to spend time re-working things
    // that I am about to throw away anyway
    //
    if (onode.children) {
        onode.children.forEach(function(child) {

            var n = nodes.findIndex(function(d) { return d.name === child.name; });
            if (n == -1) {
                console.log("Internal error: no node found for");
                console.log(onode);
                return;
            }

            // This hides the "all" node (since it doesn't have
            // an entry in the nodes array); however, this isn't
            // ideal since there are a few level=1 catagories
            // that have no descenents; with this approach they
            // just get added at the right but have no links
            // to them.
            //
            // If the "all" node is included then there's a lot of
            // visual clutter.
            //
            if (pnum >= 0) {
                var out = {source: pnum,
                           target: n,
                           value: child.size};
                output.push(out);
            }

            findLinks(child, n, nodes, output);
        });
    }
}

// For now, convert the format used for the dendogram
// into the necessary form for the Sankey plot.
//
function makeSankeyPlot(json) {

    var origInfo = json.dendogram;

    var mapInfo = { nodes: [], links: [] };

    findNodes(origInfo, mapInfo.nodes);
    mapInfo.nodes.sort(function(n1,n2) { return n1.depth > n2.depth; });
    findLinks(origInfo, -1, mapInfo.nodes, mapInfo.links);

    dummy = mapInfo;
    
  sankey
      .nodes(mapInfo.nodes)
      .links(mapInfo.links)
      // .layout(32);
      .layout(256);

  svg_sankey = d3.select("div#tree").append("svg")
        .attr("class", "sankey")  
    .attr("width", totSWidth)
    .attr("height", totSHeight)
  .append("g")
    .attr("transform", "translate(" + smargin.left + "," + smargin.top + ")");

  link_sankey = svg_sankey.append("g").selectAll(".slink")
      .data(mapInfo.links)
    .enter().append("path")
      .attr("class", "slink")
        .attr("d", path_sankey)
    /***
        .on("click", function(d) {
            // Would prefer to make this an actual link, so the UI is more
            // familiar, but this works.
            var cat = d.source.name;
            var stype = mapInfo.simbadMap[d.target.name] || "unidentified";
            if (stype == "000") { stype = "unidentified"; }
            var url = "/search/category/" +
                encodeURIComponent(cat) +
                "/" +
                encodeURIComponent(stype);
            window.location = url;
        })
    ***/
    
    /* TODO: investigate fading-out all the other nodes
        .on("mouseover", function(d) { console.log("enter"); })
        .on("mouseout", function(d) { console.log("exit"); })
    */
    
      .style("stroke-width", function(d) { return Math.max(1, d.dy); })
      .sort(function(a, b) { return b.dy - a.dy; });

  link_sankey.append("title")
        .text(function(d) {
            return d.source.name + " → " + d.target.name + "\n"
                + getNumSrcString(d.value);
        });

  var node = svg_sankey.append("g").selectAll(".node")
      .data(mapInfo.nodes)
    .enter().append("g")
      .attr("class", "snode")
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
        .text(function(d) {
            return d.name + "\n"
                + getNumSrcString(d.value);
        });

    node
    /***
        .append("a")
        .attr("xlink:href", function(d) {
            var out;
            if (mapInfo.proposals.indexOf(d.name) > -1) {
                out = "/search/category/" + encodeURIComponent(d.name);
            } else if (mapInfo.simbadNames.indexOf(d.name) > -1) {
                var stype = mapInfo.simbadMap[d.name];
                out = "/search/type/";
                if (stype == "000") {
                    out += "unidentified";
                } else {
                    out += encodeURIComponent(stype);
                }
            } else {
                out = "/error/";
            }
            return out; })
    ***/
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
    d3.select(this).attr("transform", "translate(" + d.x + "," + (d.y = Math.max(0, Math.min(sheight - d.dy, d3.event.y))) + ")");
    sankey.relayout();
    link_sankey.attr("d", path_sankey);
}

