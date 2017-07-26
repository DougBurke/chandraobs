var base = (function() {

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

    return {'hide_class': hide_class,
            'hide_nojs': function () { hide_class('nojavascript'); },
           };
    
})();
