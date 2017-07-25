
var createMapping = (function() {
    
    var margin = {top: 1, right: 1, bottom: 6, left: 1},
        width = 960 - margin.left - margin.right,
        height = 3000 - margin.top - margin.bottom;

    var totWidth = width + margin.left + margin.right;
    var totHeight = height + margin.top + margin.bottom;
    
    var color = d3.scale.category20();

    var sankey = d3.sankey()
        .nodeWidth(15)
        .nodePadding(10)
        .size([width, height]);

    var path = sankey.link();

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
        var nhours = Math.round(thours);
        var ndays = Math.floor(nhours / 24.0);
        var nweeks = Math.floor(ndays / 7);

        var tleft;

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
        var str;
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
        mapInfo.links.forEach(function(d) {
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
        
        /* TODO: investigate fading-out all the other nodes
           .on("mouseover", function(d) { console.log("enter"); })
           .on("mouseout", function(d) { console.log("exit"); })
        */
        
            .style("stroke-width", function(d) { return Math.max(1, d.dy); })
            .sort(function(a, b) { return b.dy - a.dy; });

        link.append("title")
            .text(function(d) {
                return d.source.name + " → " + d.target.name + "\n"
                    + getNumSrcString(d.numSource) + " observed for "
                    + getTimeString(d.value);
            });

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
            .style("fill", function(d) {
                d.color = color(d.name.replace(/ .*/, ""));
                return d.color; })
            .style("stroke", function(d) { return d3.rgb(d.color).darker(2); })
            .append("title")
            .text(function(d) {
                return d.name + "\n"
                    + "Observed for " + getTimeString(d.value);
            });

        node
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

    function hide_class(className) {
        var elems = document.getElementsByClassName(className);
        
        var i;
        /* hide the elements; iterate backwards to try and
         * reduce the amount of layout needed at the top of the file
         * (probably pointless)
         */
        for (i=elems.length-1; i>=0; i--) {
            elems[i].style.display = 'none';
        }
    }

    // TODO: handle the error case
    function createMapping() {

        hide_class('nojavascript');
        
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

    return createMapping;
})();
