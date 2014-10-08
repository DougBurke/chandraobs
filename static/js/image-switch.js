/*
 * Simple attempt to support user selection of the DSS/PSPC/RASS links.
 * The selection is retained as a user switches observations; this
 * may or may-not be sensible.
 */

var selectedOption = 'DSS';

function switchOption(name) {
    name = (name === undefined) ? selectedOption : name;

    if (selectedOption !== undefined) {
	document.getElementById(selectedOption).className = 'inactive';
    }
    document.getElementById(name).className = 'active';
    // it's not clear why this needs to be done; needed due to
    // move to AJAX-style interface
    document.getElementById(name + 'button').checked = true;
    selectedOption = name;
}

