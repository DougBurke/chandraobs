//
// TODO: should it be #obsid rather than use data-obsid attribute?
//
// This would then make it easier to link to from other pages,
// such as the timeline or schedule views
//

// *** setupImageBlock is not used, but left in for now ***
//
// As the image links are now re-created I no longer need to use a
// proxy for them (or, rather, some of the fun involving the
// proxies), but I'm not sure whether this is an improvement
// UI wise.
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
        if (rsp['status'] === 'success') {

            var h = rsp['navbar'] + rsp['observation'] + rsp['imglinks'];
            $mainBar.html(h);

            changeCurrentStatus(rsp['isCurrent']);
            
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
            changeCurrentStatus(false);
            $mainBar.html(rsp['error']);
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

// end
