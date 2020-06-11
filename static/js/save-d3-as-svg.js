//
// See
// https://stackoverflow.com/questions/23218174/how-do-i-save-export-an-svg-file-after-creating-an-svg-with-d3-js-ie-safari-an
// http://bl.ocks.org/Rokotyan/0556f8facbaf344507cdc45dc3622177
//
// for the code and inspiration used in this file
//

// Given a document node (assumed to be a SVG item) do something.
//
function svgToLink(svg) {
  const node = svg.cloneNode(true);
  node.setAttribute('xlink', 'http://www.w3.org/1999/xlink');

  const styles = getStyles(node);
  addStyles(styles, node);

  const serializer = new XMLSerializer();
  let out = serializer.serializeToString(node);
  
  // Fix root xlink without namespace
  out = out.replace(/(\w+)?:?xlink=/g, 'xmlns:xlink=');
  
  // Safari NS namespace fix
  out = out.replace(/NS\d+:href/g, 'xlink:href');

  /***

This is from similar code

  //add name spaces.
if(!source.match(/^<svg[^>]+xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)){
    source = source.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
}
if(!source.match(/^<svg[^>]+"http\:\/\/www\.w3\.org\/1999\/xlink"/)){
    source = source.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
}

//add xml declaration
source = '<?xml version="1.0" standalone="no"?>\r\n' + source;
***/

  return "data:image/svg+xml;charset=utf-8," + encodeURIComponent(source);
}

function contains(str, arr) {
  return arr.indexOf(str) !== -1;
}

function getStyles(element) {
  const selectors = [];
  
  // Add Parent element Id and Classes to the list
  selectors.push('#' + element.id);
  for (let className of element.classList) {
    const selector = '.' + className;
    if (!contains(selector, selectors)) {
      selectors.push(selector);
    }
  }
  
  // Add Children element Ids and Classes to the list
  const nodes = element.getElementsByTagName("*");
  for (let node of nodes) {
    const selector = '#' + node.id;
    if (!contains(selector, selectors)) {
      selectors.push(selector);
    }

    for (let className of node.classList) {
      const selector2 = '.' + className;
      if (!contains(selector2, selectors)) {
	selectors.push(selector2);
      }
    }
  }
    
  // Extract CSS Rules
  var out = "";
  for (let sheet of document.styleSheets) {
    try {
      if (!sheet.cssRules) { continue; }
    } catch (e) {
      if (e.name !== 'SecurityError') { throw e; }
      continue;
    }

    for (let rule of sheet.cssRules) {
      if (!contains(rule.selectorText, selectors)) { continue; }
      out += rule.cssText;
    }
  }

  return out;
}


// Add the styles (given as a string) to the element.
//
function addStyles(cssText, element) {
  const style = document.createElement("style");
  style.setAttribute("type","text/css"); 
  style.innerHTML = cssText;
  
  const refNode = element.hasChildNodes() ? element.children[0] : null;
  element.insertBefore(style, refNode);
}

