/*
 * The spinner CSS and HTML is taken from http://tobiasahlin.com/spinkit/
 * which is released under a MIT license.
 *
 * The code here just makes it easier to add/remove the spinner in a page.
 */

const spinner = (function() {

    function addDiv(parent, className) {
	const div = document.createElement('div');
	div.setAttribute('class', className);
	parent.appendChild(div);
    }
    
    // Create a spinner div in the page, with class spinner, and return it.
    //
    function createSpinner() {
	const spinner = document.createElement('div');
	spinner.setAttribute('class', 'spinner');
	
	addDiv(spinner, 'rect1');
	addDiv(spinner, 'rect2');
	addDiv(spinner, 'rect3');
	addDiv(spinner, 'rect4');
	addDiv(spinner, 'rect5');

	return spinner;
    }

    return { createSpinner: createSpinner };
	  
})();
