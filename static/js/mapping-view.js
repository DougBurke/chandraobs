"use strict";

// The baseObj argument provides the hide_nojs() routine.
// I should just make that a pre-requisite for calling createMap
// but leave that for a later revision.
//
const createMapping = (function(baseObj) {
    
    const margin = {top: 1, right: 1, bottom: 6, left: 1},
        width = 960 - margin.left - margin.right,
        height = 3000 - margin.top - margin.bottom;

    const totWidth = width + margin.left + margin.right;
    const totHeight = height + margin.top + margin.bottom;

    // It's probably worth re-thinking the color scheme
    const color = d3.scaleOrdinal(d3.schemePaired);

    const sankey = d3.sankey()
          .nodeWidth(15)
          .nodePadding(10)
          .size([width, height]);

    const path = sankey.link();

    var svg, link;

    // This replicates logic in lib/Types.h::showExpTime but
    // it's not quite the same, in that they are optimised
    // for different time ranges, and so the units are
    // different: seconds, minutes, hours, days
    // compared to hours, days, and weeks.
    //
    // I have decided to just hard-code limits rather than
    // doing it sensibly (i.e. with an algorithm) as 
    // I can not be bothered to investigate the
    // portability of modulo-arithmetic across JavaScript
    // engines, and the repition below isn't terrible.
    //
    function getTimeString(thours) {
        // Do these outside the checks below, even if not needed
        // in all cases
        const nhours = Math.round(thours);
        const ndays = Math.floor(nhours / 24.0);
        const nweeks = Math.floor(ndays / 7);

        let tleft;
        let str;
        
        if (nhours < 1) {
            str = "< 1 hour";
        } else if (nhours < 2) {
            str = "1 hour";
        } else if (nhours < 24) {
            str = nhours + " hours";
        } else if (ndays < 7) {
            str = ndays + " day";
            if (ndays > 1) {
                str += "s";
            }
            tleft = nhours - ndays * 24;
            if (tleft > 0) {
                str += " and " + tleft + " hour";
                if (tleft > 1) { str += "s"; }
            }
        } else {
            str = nweeks + " week";
            if (nweeks > 1) { str += "s"; }

            tleft = ndays - nweeks * 7;
            if (tleft > 0) {
                str += " and " + tleft + " day";
                if (tleft > 1) { str += "s"; }
            }
        }
        
        return str;
    }

    function getNumSrcString(nsrc) {
        let str;
        if (nsrc == 1) {
            str = "One source";
        } else {
            str = nsrc + " sources";
        }
        return str;
    }
    
    function makePlot(mapInfo) {

        // for now, hard-code the value to be the totalExp field,
        // but convert from kilo-seconds to hours.
        //  
        mapInfo.links.forEach((d) => {
            d.value = d.totalExp * 1000.0 / 3600.0;
        });
        
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
            .on("click", (d) => {
                // Would prefer to make this an actual link, so the UI is more
                // familiar, but this works.
                const cat = d.source.name;
                let stype = mapInfo.simbadMap[d.target.name] || "unidentified";
                if (stype == "000") { stype = "unidentified"; }
                window.location = "/search/category/" +
                    encodeURIComponent(cat) +
                    "/" +
                    encodeURIComponent(stype);
            })
        
        /* TODO: investigate fading-out all the other nodes
           .on("mouseover", function(d) { console.log("enter"); })
           .on("mouseout", function(d) { console.log("exit"); })
        */
        
            .style("stroke-width", (d) => { return Math.max(1, d.dy); })
            .sort((a, b) => { return b.dy - a.dy; });

        link.append("title")
            .text((d) => {
                return d.source.name + " → " + d.target.name + "\n"
                    + getNumSrcString(d.numSource) + " observed for "
                    + getTimeString(d.value);
            });

        const node = svg.append("g").selectAll(".node")
            .data(mapInfo.nodes)
            .enter().append("g")
            .attr("class", "node")
            .attr("transform", (d) => { return "translate(" + d.x + "," + d.y + ")"; })
            .call(d3.drag()
                  .subject((d) => { return d; })
		  // looks like I have broken this code, since 'this' is no
		  // longer the correct element; did I break some implicit
		  // when refactoing?
		  /// DO we actually need this?
                  // .on("start", () => { this.parentNode.appendChild(this); })

		  // It's not clear to me what this is doing?
                  .on("start", function () { this.parentNode.appendChild(this); })
		  
                  .on("drag", dragmove));

        node.append("rect")
            .attr("height", (d) => { return d.dy; })
            .attr("width", sankey.nodeWidth())
            .style("fill", (d) => {
                d.color = color(d.name.replace(/ .*/, ""));
                return d.color; })
            .style("stroke", (d) => { return d3.rgb(d.color).darker(2); })
            .append("title")
            .text((d) => {
                return d.name + "\n"
                    + "Observed for " + getTimeString(d.value);
            });

        node
            .append("a")
            .attr("xlink:href", (d) => {
                let out;
                if (mapInfo.proposals.indexOf(d.name) > -1) {
                    out = "/search/category/" + encodeURIComponent(d.name);
                } else if (mapInfo.simbadNames.indexOf(d.name) > -1) {
                    const stype = mapInfo.simbadMap[d.name];
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
            .append("text")
            .attr("x", -6)
            .attr("y", (d) => { return d.dy / 2; })
            .attr("dy", ".35em")
            .attr("text-anchor", "end")
            .attr("transform", null)
            .text((d) => { return d.name; })
            .filter((d) => { return d.x < width / 2; })
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

        baseObj.hide_nojs();
        
        // add in animation; this was taken from
        // http://ajaxload.info/
        //
        // also, should try and pre-load it
        //
        // TODO: better center the image in the width of the SVG element
        $('#mapping').html('<div class="waiting"><img width=100 height=100 src="/img/loading.gif" alt="Loading"></div>');
        
        $.ajax({url: '/api/mappings'})
            .done(makePlot);
    }

    return createMapping;
})(base);
