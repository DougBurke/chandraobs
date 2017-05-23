/*
 * Render the SIMBAD types using the hiearchy from
 * http://cds.u-strasbg.fr/cgi-bin/Otype?X
 * based on the d3 example at
 * https://bl.ocks.org/mbostock/4339083
 */

var root;
var duration = 750;

var totWidth = 1200;
var totHeight = 3500;

var margin = {top: 20, right: 120, bottom: 20, left: 120},
    width = totWidth - margin.right - margin.left,
    height = totHeight - margin.top - margin.bottom;

var baseOpacity = 1;
var unselOpacity = 0.5;

var tree;
var svg;
var diagonal;

// Toggle children on click.
//
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

// Based on http://bl.ocks.org/mbostock/4062006
function changeOtherNodes(opacity) {
    return function(g, i) {
        // What shall we fade? I don't quite understand what is going
        // on with the opacity, but changing both opacity and fill-opacity
        // appears to have fixed, or worked around, the issue
        // of text not being restored to full opacity after being hidden
        // and restored.
        //
        // might it be nice to keep the parent/children?
        svg.selectAll(".node text")
            .filter(function(d) { return d.id != g.id; })
            .transition()
            .style("fill-opacity", opacity)
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
        .on("mouseover", changeOtherNodes(unselOpacity))
        .on("mouseout", changeOtherNodes(baseOpacity))
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

function createTree(json) {

    if (!json || json.length === 0) { console.log("No tree data!"); return; }

    // filter out all nodes with a size of zero; assume that the
    // top-level element is not empty.
    function removeEmpty(d) {
        if (d.size === 0) { return 0; }
        if (d.children) {
            d.children = d.children.filter(removeEmpty);
        }
        return 1;
    }
    json.children = json.children.filter(removeEmpty);

    svg = d3.select("div#tree").append("svg")
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

    // d3.select(self.frameElement).style("height", "800px");
    d3.select("#tree").style("height", totHeight + "px");
}
