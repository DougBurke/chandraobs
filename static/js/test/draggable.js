/*
 * Basic support for draggable "windows" (aka panes)
 */

const draggable = (function () {

    const paneMimeType = "application/x-pane+json";

    // Perhaps I should just always use clientX/Y rather than this?
    //
    function getEventPos(event) {
        var x, y;
        if (typeof event.x === "undefined") {
            x = event.clientX;
            y = event.clientY;
        } else {
            x = event.x;
            y = event.y;
        }
        return {x: x, y: y};
    }

    // Initialize the drag, storing the start location.
    //
    function startDrag(event) {

	// If this comes from any range sliders then cancel the panel
	// drag. See https://stackoverflow.com/a/20819717
	// Note that for now I have turned off drag support for the
	// button panel, which means this isn't needed, but left
	// in (there seemed to be some problem with events still
	// bubbling through even with the following and did not
	// want to investigate too much).
	//
	if (document.activeElement.tagName === "INPUT") {
	    return false;
	}

        var toMove = event.target.id;
        
        // Store the current location of the event, so we can find out
        // how much we have shifted by.
        //
        var evpos = getEventPos(event);
        evpos.id = toMove;
        event.dataTransfer.setData(paneMimeType, JSON.stringify(evpos));

        // The id is not supposed to contain a space, and we don't, so
        // it's okay to separate with spaces.
        //
        var store = evpos.x.toString() + " " + evpos.y.toString() +
            " " + toMove;
        event.dataTransfer.setData("text/plain", store);

        // All we want to do is change the position of the div,
        // but do not change the DOM at all. So it's not clear
        // what the best value here is.
        //
        event.dataTransfer.effectAllowed = "move";
        event.dataTransfer.dropEffect = "move";
    }

    // Note that the way the movement happens, the position is
    // remembered when the pane is deleted and then re-shown.
    // This seems to be okay from a usability viewpoint, but
    // may need to be thought about at a later date.
    //
    function stopDrag(event) {

        // It's not clear whether I can guarantee that the structured
        // data is available, so code up a fallback, just in case.
        //
        var x0, y0, toMove;
        var fallback = true;
        var types = event.dataTransfer.types;
        for (var i=0; i < types.length; i++) {
            if (types[i] === paneMimeType) {
                fallback = false;
                break;
            }
        }
        if (fallback) {
            var store = event.dataTransfer.getData('text/plain');
            var tags = store.split(" ");
            if (tags.length !== 3) {
                console.log("Invalid store data from drop: [" + store + "]");
                event.preventDefault();  // is this sensible here?
                return;
            }
            x0 = parseInt(tags[0]);
            y0 = parseInt(tags[1]);
            toMove = tags[2];
        } else {
            var json = event.dataTransfer.getData(paneMimeType);
            var obj = JSON.parse(json);
            x0 = obj.x;
            y0 = obj.y;
            toMove = obj.id;
        }
            
        var evpos = getEventPos(event);
        var dx = evpos.x - x0;
        var dy = evpos.y - y0;

        var div = document.getElementById(toMove);
        var stackbbox = div.getBoundingClientRect();
        var parentbbox = div.parentElement.getBoundingClientRect();

        var x1 = div.offsetLeft + dx;
        var y1 = div.offsetTop + dy;

        var xmin = 0, xmax = parentbbox.width - stackbbox.width;
        var ymin = 0, ymax = parentbbox.height - stackbbox.height;
        if (x1 < xmin) { x1 = xmin; } else if (x1 > xmax) { x1 = xmax; }
        if (y1 < ymin) { y1 = ymin; } else if (y1 > ymax) { y1 = ymax; }

        div.style.left = x1.toString() + "px";
        div.style.top = y1.toString() + "px";

        // For some reason the width could change, so let's be explicit
        // and also add in the other values. My original thought was that
        // there was something strange going on because the initial
        // placement of the stackinfo div uses top/right, but the move
        // changes top/left. Experiments weren't conclusive.
        //
	// Maybe I am not using the correct width/height here?
	//
	// I have no idea what the original problem was, but the following
	// seems to cause problems too.
	//
	/***
        if (toMove === "stackinfo") {
            div.style.right = (parentbbox.width - x1 - stackbbox.width).toString()
                + "px";
            div.style.bottom = (parentbbox.height - y1 - stackbbox.height).toString()
                + "px";
        }
        ***/

        event.preventDefault();

    }

    return {startDrag: startDrag,
	    stopDrag: stopDrag};

})();
