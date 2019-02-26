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
	//
	// This shouldn't be needed just yet but left as a comment in
	// case I add a range slider.
	//
	/***
	if (document.activeElement.tagName === "INPUT") {
	    return false;
	}
	***/

        const toMove = event.target.id;
        
        // Store the current location of the event, so we can find out
        // how much we have shifted by.
        //
        const evpos = getEventPos(event);
        evpos.id = toMove;
        event.dataTransfer.setData(paneMimeType, JSON.stringify(evpos));

        // The id is not supposed to contain a space, and we don't, so
        // it's okay to separate with spaces.
        //
        const store = evpos.x.toString() + " " + evpos.y.toString() +
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
        const types = event.dataTransfer.types;
        for (var i=0; i < types.length; i++) {
            if (types[i] === paneMimeType) {
                fallback = false;
                break;
            }
        }
        if (fallback) {
            const store = event.dataTransfer.getData('text/plain');
            const tags = store.split(" ");
            if (tags.length !== 3) {
                console.log("Invalid store data from drop: [" + store + "]");
                event.preventDefault();  // is this sensible here?
                return;
            }
            x0 = parseInt(tags[0]);
            y0 = parseInt(tags[1]);
            toMove = tags[2];
        } else {
            const json = event.dataTransfer.getData(paneMimeType);
            const obj = JSON.parse(json);
            x0 = obj.x;
            y0 = obj.y;
            toMove = obj.id;
        }
            
        const evpos = getEventPos(event);
        const dx = evpos.x - x0;
        const dy = evpos.y - y0;

	// This is not expected to fail, so do not try to do much
	// if it does.
	//
	const div = document.querySelector('#' + toMove);
	if (div === null) {
	    console.log("INTERNAL ERROR: unable to find div I'm dragging!");
	    return;
	}

        const stackbbox = div.getBoundingClientRect();
        const parentbbox = div.parentElement.getBoundingClientRect();

        var x1 = div.offsetLeft + dx;
        var y1 = div.offsetTop + dy;

        const xmin = 0, xmax = parentbbox.width - stackbbox.width;
        const ymin = 0, ymax = parentbbox.height - stackbbox.height;
        if (x1 < xmin) { x1 = xmin; } else if (x1 > xmax) { x1 = xmax; }
        if (y1 < ymin) { y1 = ymin; } else if (y1 > ymax) { y1 = ymax; }

	// Since the div was originally placed with respect to the bottom
	// retain that here.
	//
        div.style.left = x1.toString() + "px";
        // div.style.top = y1.toString() + "px";
        div.style.bottom = (ymax - y1).toString() + "px";

        event.preventDefault();

    }

    return {startDrag: startDrag,
	    stopDrag: stopDrag};

})();
