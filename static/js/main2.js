//
// As the image links are now re-created I no longer need to use a
// proxy for them (or, rather, some of the fun involving the
// proxies), but I'm not sure whether this is an improvement
// UI wise.
//

// *** this is not used, but left in for now ***
//
// Set up the links/images/text for the image block and show it.
//
// How do we want to handle image loading?
//
function setupImageBlock(obsdata, sobs, mprop) {

    // what happens if we just "break" the image, i.e. have no source
    // - it doesn't seem, visually, too dis-pleasing on a local
    //   connection; maybe worse with more latency?
    // var loadimg = 'loading.gif';
    var loadimg = ''; 

    $( '#DSS'  ).prop('src', loadimg);
    $( '#RASS' ).prop('src', loadimg);
    $( '#PSPC' ).prop('src', loadimg);

    var path = obsdata.sequence + '/' + obsdata.obsid;
    $.ajax({
	url: '/proxy/dss/' + path
	, type: 'GET'
	, dataType: 'text'
	, cache: true
	}).done(function(d64) {
	    $( '#DSS' ).prop('src', 'data:image/gif;base64,' + d64);
	});

    $.ajax({
	url: '/proxy/rass/' + path
	, type: 'GET'
	, dataType: 'text'
	, cache: true
	}).done(function(d64) {
	    $( '#RASS' ).prop('src', 'data:image/gif;base64,' + d64);
	});

    $.ajax({
	url: '/proxy/pspc/' + path
	, type: 'GET'
	, dataType: 'text'
	, cache: true
	}).done(function(d64) {
	    $( '#PSPC' ).prop('src', 'data:image/gif;base64,' + d64);
	});

    // TODO: could this instead be included on this page?
    $( '#WWTbutton' ).prop('href', '/obsid/' + obsdata.obsid + '/wwt')

    renderObsIdDetails(obsdata, sobs, mprop);
    switchOption();
}

var dummy;
function showObsId(obsid) {
    $.ajax({
	url: "/api/page/" + obsid,
	dataType: "json"
    }).done(function (rsp) {
        var $mainBar = $( '#mainBar' );
        if (rsp['status'] === 'success') {

            var h = rsp['navbar'] + rsp['observation'] + rsp['imglinks'];
            $mainBar.html(h);

            if (rsp['isCurrent']) {
                $( '#currentLink' ).addClass('chosen');
            } else {
                $( '#currentLink' ).removeClass('chosen');
            }
            
            var $nav = $( '#obslinks' );
            var $p = $nav.find( 'li.prevLink' );
            var $n = $nav.find( 'li.nextLink');

            $p.click(function (e) { showObsId($p.data('obsid')); });
            $n.click(function (e) { showObsId($n.data('obsid')); });

            /* 
             * add click handlers to the obsid links in the
             * rsp['observation'] section
             */
            $mainBar.find( 'a.obsidlink' ).click(function (e) {
                showObsId(this.getAttribute('data-obsid'));
            });
               
        } else {
            /* TODO: more here? */
            $mainBar.html(rsp['error']);
            $( '#currentLink' ).removeClass('chosen');
        }
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
            /***
             *** WHAT TO DO ON ERROR HERE
             ***/
        }
    });
}

function initialize() {
    showCurrent();
}

// end
