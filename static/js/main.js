/*
 * Simple attempt to support user selection of the DSS/PSPC/RASS links
 */

// Switch to name='PSPC', 'RASS', or 'DSS'
// This does not check that the image has loaded, and
// it would be nice to be an animated fade.
var selectedImage = 'DSS';
// var selectedButton = undefined;

function switchImage(name) {
  if (selectedImage !== undefined) {
    // document.getElementById(selectedImage).style = 'display: none';
    document.getElementById(selectedImage).className = 'inactive';
    // document.getElementById(selectedButton).className = 'inactive';
  }
  // var button = name + "button";
  // document.getElementById(name).style = 'display: block';
  document.getElementById(name).className = 'active';
  // document.getElementById(button).className = 'active';
  selectedImage = name;
  // selectedButton = button;
}

// document onload
function initialize() {
}
