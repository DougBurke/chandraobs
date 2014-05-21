/*
 * Simple attempt to support user selection of the DSS/PSPC/RASS links
 */

// Switch to name='PSPC', 'RASS', 'DSS' or 'Details'
// This does not check that the image has loaded, and
// it would be nice to be an animated fade.
var selectedOption = 'DSS';

function switchOption(name) {
  if (selectedOption !== undefined) {
    document.getElementById(selectedOption).className = 'inactive';
  }
  document.getElementById(name).className = 'active';
  selectedOption = name;
}

// document onload
function initialize() {
}
