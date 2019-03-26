"use strict";

//
// Render the SIMBAD types using the hiearchy from
// http://cds.u-strasbg.fr/cgi-bin/Otype?X
// based on the d3 example at
// https://bl.ocks.org/mbostock/4339083
// which has become
// https://observablehq.com/@d3/collapsible-tree
//

const createTree = (function () {
    
    const totWidth = 1400;
    const totHeight = 3500;

    const margin = {top: 20, right: 120, bottom: 20, left: 120},
	  width = totWidth - margin.right - margin.left,
	  height = totHeight - margin.top - margin.bottom;

    const baseOpacity = 1;
    const unselOpacity = 0.5;
    
    const dx = 20;
    const dy = (width / 6);

    const tree = d3.tree().nodeSize([dx, dy])

    const diagonal = d3.linkHorizontal().x(d => d.y).y(d => d.x);

    var root;
    var svg;
    var gNode;
    var gLink;
    
    // Based on http://bl.ocks.org/mbostock/4062006
    //
    // I think it might be good to change the non-children of the
    // node (so that the children retain the same opacity as the
    // selected node), but that's for a later experiment.
    //
    function changeOtherNodes(opacity) {
	return (g, i) => {
            // What shall we fade? I don't quite understand what is going
            // on with the opacity, but changing both opacity and fill-opacity
            // appears to have fixed, or worked around, the issue
            // of text not being restored to full opacity after being hidden
            // and restored.
            //
            svg.selectAll(".node text")
		.filter(d => d.id != g.id)
		.transition()
		.style("fill-opacity", opacity)
		.style("opacity", opacity);
	};
    }

    // Filter out those nodes with a size of zero.
    //
    function removeEmpty(d) {
	if (d.size === 0) { return false; }
	if (d.children) {
	    d.children = d.children.filter(removeEmpty);
	}
	return true;
    }

    function update(source) {
	const duration = d3.event && d3.event.altKey ? 2500 : 250;
	const nodes = root.descendants().reverse();
	const links = root.links();

	// Compute the new tree layout.
	tree(root);

	let left = root;
	let right = root;
	root.eachBefore(node => {
	    if (node.x < left.x) left = node;
	    if (node.x > right.x) right = node;
	});

	const height = right.x - left.x + margin.top + margin.bottom;

	const transition = svg.transition()
	      .duration(duration)
	      .attr("height", height)
	      .attr("viewBox",
		    [-margin.left, left.x - margin.top, width, height])
	      .tween("resize",
		     window.ResizeObserver ? null :
		     () => () => svg.dispatch("toggle"));

	// Update the nodes…
	const node = gNode.selectAll("g")
	      .data(nodes, d => d.id);

	const nodeFill = d => d._children ? "lightsteelblue" : "#fff";
	
	// Enter any new nodes at the parent's previous position.
	//
	// Note that the click event is placed on the circle only,
	// not the whole node.
	//
	const nodeEnter = node.enter().append("g")
	      .attr("class", "node")
	      .attr("transform", d => `translate(${source.y0},${source.x0})`)
	      .attr("fill-opacity", 0)
	      .attr("stroke-opacity", 0)
	      .on("mouseover", changeOtherNodes(unselOpacity))
	      .on("mouseout", changeOtherNodes(baseOpacity));

	nodeEnter.append("circle")
	    .attr("r", 1e-6)
	    .attr("fill", nodeFill)
	    .on("click", d => {
		d.children = d.children ? null : d._children;
		update(d);
	    });

	// Note: top level ("all") has no data.size/searchLink field
	//       How do we avoid adding a link in this case?
	//
	// Note: somehow I'm generating at least 2 text elements
	//       for the node, so I'm doing something screwy
	//       (a remnant of converting from d3 v2-ish to d3 v5?)
	//
	nodeEnter
	    .append("a")
	    .attr("xlink:href", d => d.data.searchLink)
	    .append("text")
	    .attr("dy", "0.35em")
	    .attr("x", d => d._children ? -10 : 10)
	    .attr("text-anchor", d => d._children ? "end" : "start")
	    .text(d => {
		let lbl = d.data.name;
		if (d.data.size > 0) { lbl += " (" + d.data.size + ")"; }
		return lbl;
	    })
	    .clone(true).lower()
	    .attr("stroke-linejoin", "round")
	    .attr("stroke-width", 3)
	    .attr("stroke", "white")
	    .attr("fill-opacity", 1e-6);

	// Transition nodes to their new position.
	const nodeUpdate = node.merge(nodeEnter).transition(transition)
	      .attr("transform", d => `translate(${d.y},${d.x})`)
	      .attr("fill-opacity", 1)
	      .attr("stroke-opacity", 1);

	// Is this needed (was used in a previous version)
	nodeUpdate.select("circle")
	    .attr("r", 4.5)
	    .style("fill", nodeFill);
	
	// Transition exiting nodes to the parent's new position.
	const nodeExit = node.exit().transition(transition).remove()
	      .attr("transform", d => `translate(${source.y},${source.x})`)
	      .attr("fill-opacity", 0)
	      .attr("stroke-opacity", 0);

	// Update the links…
	const link = gLink.selectAll("path.link")
	      .data(links, d => d.target.id);

	// Enter any new links at the parent's previous position.
	const linkEnter = link.enter().append("path")
	      .attr("class", "link")
	      .attr("d", d => {
		  const o = {x: source.x0, y: source.y0};
		  return diagonal({source: o, target: o});
	      });

	// Transition links to their new position.
	link.merge(linkEnter).transition(transition)
	    .attr("d", diagonal);

	// Transition exiting nodes to the parent's new position.
	link.exit().transition(transition).remove()
	    .attr("d", d => {
		const o = {x: source.x, y: source.y};
		return diagonal({source: o, target: o});
	    });
	    
	// Stash the old positions for transition.
	root.eachBefore(d => {
	    d.x0 = d.x;
	    d.y0 = d.y;
	});
    }

    function createTree(json) {
	if (!json || json.length === 0) {
	    console.log("No tree data!");
	    return;
	}

	// Remove those elements of the SIMBAD hierarchy which
	// have no matching Chandra data. It might be nice to see
	// them, but adds a bit of "noise" to the figure.
	//
	json.children = json.children.filter(removeEmpty);
	root = d3.hierarchy(json);

	root.x0 = 10;
	root.y0 = 0;
	root.descendants().forEach((d, i) => {
	    d.id = i;
	    d._children = d.children;
	});

	svg = d3.select('div#tree')
	    .append("svg")
	    .attr("width", width)
	    .attr("height", dx)
	    .attr("viewBox", [-margin.left, -margin.top, width, dx])
	    .style("font", "10px sans-serif")
	    .style("user-select", "none");

	gLink = svg.append("g")
	    .attr("fill", "none")
	    .attr("stroke", "#555")
	    .attr("stroke-opacity", 0.4)
	    .attr("stroke-width", 1.5);

	gNode = svg.append("g")
	    .attr("cursor", "pointer");

	update(root);

	// return svg.node();
    }

    
    return createTree;

})();
