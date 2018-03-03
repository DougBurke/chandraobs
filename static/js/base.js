"use strict";

var base = (function() {

    function hide_class(className) {
        for (const elem of document.getElementsByClassName(className)) {
            elem.style.display = 'none';
        }
    }

    return {'hide_class': hide_class,
            'hide_nojs': () => { hide_class('nojavascript'); },
           };
    
})();
