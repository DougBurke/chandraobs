//
// TODO: should it be #obsid rather than use data-obsid attribute?
//
// This would then make it easier to link to from other pages,
// such as the timeline or schedule views
//

var main2 = (function() {

    /*
     * Show or hide the graphical elements that indicate whether
     * this is the current observation or not.
     */
    function changeCurrentStatus(isCurrent) {
        if (isCurrent) {
            $( '#home' ).addClass('chosen');
        } else {
            $( '#home' ).removeClass('chosen');
        }
    }
    
    var noServerPara =
        '<p class="noserver">' +
        'It looks like the web server has shut ' +
        'down. Try re-loading the page, as it ' +
        'may be due to the Heroku application ' +
        'shutting down. If this is the case it ' +
        'could take a short time for things to come ' +
        'back on-line.' +
        '</p>';
    
    function serverGoneByBy() {
        changeCurrentStatus(false);
        $( '#mainBar' ).html(noServerPara);
    }

    function showObsId(obsid) {
        $.ajax({
	    url: "/api/page/" + obsid,
	    dataType: "json"
        }).done(function (rsp) {
            var $mainBar = $( '#mainBar' );
            if (rsp.status === 'success') {
                
                var h = rsp.navbar + rsp.observation + rsp.imglinks;
                $mainBar.html(h);
                
                changeCurrentStatus(rsp.isCurrent);
                
                var $nav = $( '#obslinks' );
                var $p = $nav.find( 'li.prevLink' );
                var $n = $nav.find( 'li.nextLink');

                $p.click(function (e) { showObsId($p.data('obsid')); });
                $n.click(function (e) { showObsId($n.data('obsid')); });

                /* indicate that these are clickable */
                $p.css('cursor', 'pointer');
                $n.css('cursor', 'pointer');

                /* 
                 * add click handlers to the obsid links in the
                 * rsp['observation'] section
                 */
                $mainBar.find( 'a.obsidlink' ).click(function (e) {
                    showObsId(this.getAttribute('data-obsid'));
                });

                /*
                 * Prepare WWT widget for the new page. It does not
                 * seem to be working.
                 */
                wwt.resetStatus();

            } else {
                changeCurrentStatus(false);
                $mainBar.html(rsp.error);
            }
        }).fail(function(xhr, status, e) {
            serverGoneByBy();
        });
    }

    function showCurrent() {
        $.ajax({
            url: "/api/current",
            dataType: "json"
        }).done(function(rsp) {
            if (rsp[0] === 'Success') {
                showObsId(rsp[1]);
            } else {
                changeCurrentStatus(false);
                $( '#mainBar' ).html('<p class="unknownerror">' +
                                     'There has been an error somewhere ' +
                                     'and I do not know what to do :-(' +
                                     '</p>');
            }
        }).fail(function(xhr, status, e) {
            serverGoneByBy();
        });
    }

    function initialize() {
        /* set up the 'What is Chandra doing now?' link */
        $( 'a#home' ).attr('href', '#');
        $( 'a#home' ).click(function (e) {
            showCurrent();
        });
        showCurrent();
    }
    
    return {initialize: initialize};

})();

// end
