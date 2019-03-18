"use strict";

// can't make strict for now
var base = (function() {

    function hide_class(className) {
	document.querySelectorAll('.' + className).forEach((el) => {
	    el.style.display = 'none';
        });
    }

    return {'hide_class': hide_class,
            'hide_nojs': () => { hide_class('nojavascript'); },
           };
    
})();
