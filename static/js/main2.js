// Render the information from the server. For now try and re-create
// the Haskell code that generates the page segments, to look at
// information flow. An easier alternative would be to just ask
// the server for the HTML blocks, but that requires re-compiling
// the code, so leave for now. The approach taken here may be useful
// if I decide to go for a more template-based approach, since it
// shows what structures we can use as templates.
//
// It's complicated by the Haskell structure "infecting"
// the JSON.

// TODO: obsid 17169 is in the schedule when it probably shouldn't be

// TODO: better error handling (in particular if ajax calls fail)
//

// TODO: clean up the page transitions as it's not ideal at the
//       moment.

// TODO: image loading should be asynchronous so users can
//       be told when things are ready
//       ALSO: need to decide between .hide and class: inactive
//             approach to hide/show
//             - have put in fade in/out but not convinced this is a
//               good approach since really should be fadeing the text
//               + images, not the two parts individually...
//               

// TODO:
//  - should page be re-generated every minute (or so) so that the
//    time display is current (ish)?
//    Then it would also be able to update when observations change
//    Also could update the current-observation counter; this could
//    be done without the former, in which case either a fixed delta
//    or just determine what the time until refresh is needed
//    (although what happens when a user comes back to a page after
//     the computer going to sleep?)
//

var _nextObsId = undefined;
var _prevObsId = undefined;
var _currObsId = undefined;

// TODO: handle case when next/prev is undefined, although a user should never
// be able to select the link in this case
function setupNavBarOnClick() {
    var $nav = $( '#obslinks' );
    var $p = $nav.find( 'li.prevLink' );
    var $n = $nav.find( 'li.nextLink' );
    // do not use obsidClick here as want _next/prev to change
    $p.click(function (e) {
	$.ajax({
	    url: "/api/obsid/" + _prevObsId,
	    dataType: "json"
	}).done(renderObsInfo);
    });
    $n.click(function (e) {
	$.ajax({
	    url: "/api/obsid/" + _nextObsId,
	    dataType: "json"
	}).done(renderObsInfo);
    });
}

// Since _currObsId is not being updated regularly -
// at least not at the moment - check again here.
//
function setupCurrentClick() {
    var $nav = $( '#currentLink' );
    $nav.click(function (e) {
	if (_currObsId === undefined) {
	    // what here?
	    console.log("TODO: reload the page");
	} else {
	    renderCurrent();
	}
    });
}


// Extract the ObsId value (as an integer) and the target
// name from a Record
// structure encoded in JSON. undefined is returned if
// there is no ObsId field.
//
function _getObsIdName(record) {
    if (record === undefined) {
	return undefined;
    }
    if ("Left" in record) {
	record = record["Left"];
    } else if ("Right" in record) {
	record = record["Right"];
    } else {
	return undefined;
    }
    var obsid;
    if (("ObsId" in record) && ("ObsId" in record["ObsId"])) {
	obsid = record["ObsId"]["ObsId"];
    } else {
	obsid = undefined;
    }
    var target;
    if (("Target" in record)) {
	target = record["Target"];
    } else {
	target = undefined;
    }
    return [obsid, target];
}

function _getObsId(record) {
    var out = _getObsIdName(record);
    return (out === undefined) ? undefined : out[0];
}

// Convert instrument and grating settings to something readable.
function _toInstrument(inst) {
    if (inst === "aciss")      { return "ACIS-S"; }
    else if (inst === "acisi") { return "ACIS-I"; }
    else if (inst === "hrci")  { return "HRC-I"; }
    else if (inst === "hrcs")  { return "HRC-S"; }
    else { return inst; }
}

function _toGrating(grat) {
    if (grat === "none")      { return "NONE"; }
    else if (grat === "hetg") { return "HETG"; }
    else if (grat === "letg") { return "LETG"; }
    else { return grat; }
}

// return a string of the list contents separated
// by , but using an Oxford comma at the end.
//
function addList(xs) {

    if (xs.length === 0) { return ""; }
    else if (xs.length === 1) { return xs[0]; }
    else if (xs.length === 2) { return xs[0] + " and " + xs[1]; }
    else {
	return xs.slice(0, xs.length-2).join(", ") + xs[xs.length-2] + " and " +
	    xs[xs.length-1];
    }
}

// Return the list of other observations that are joint with this one.
var _missions = ["HST", "NOAO", "NRAO", "RXTE", "Spitzer", "Suzaku", "XMM", "Swift", "NuStar"];

// Note: based on Haskell version
//
// timeConv maps from ks into a string.
function getJointObs(so, timeConv) {
    return _missions.map(function(d) { return [d, so["Joint" + d.toUpperCase()]]; })
               .filter(function(d) { return d[1] !== null; })
               .map(function(d) { return d[0] + " (for " + timeConv(d[1]['S']) + ")"; });
}

// Remove the "CXO-" prefix from the joint-with field since
// there are at least two cases of "CXO-HST".
function cleanJointName(j) {
    if (j.startsWith("CXO-")) { return j.slice(4); }
    else                      { return j; }
}

// Note that this also sets up the _nextObsId and _prevObsId
// fields, as well as the 'current obs' link in the
// header, so the name isn't perfect.
//
function renderNavBar(obsinfo) {
    var prevObs = obsinfo.PrevObs;
    var nextObs = obsinfo.NextObs;

    if ((_currObsId !== undefined) &&
	(_currObsId === _getObsId(obsinfo.CurrentObs))) {
	$( '#currentLink' ).addClass('chosen');
    } else {
	$( '#currentLink' ).removeClass('chosen');
    }

    var $nav = $( '#obslinks' );
    var $p = $nav.find( 'li.prevLink' );
    var $n = $nav.find( 'li.nextLink' );
    var res, lbl;
    if (prevObs !== null) {
	res = _getObsIdName(prevObs);
	if (res === undefined) {
	    _prevObsId = undefined;
	    // $p.hide();
	    $p.fadeOut();
	} else {
	    _prevObsId = res[0];
	    lbl = (res[1] === undefined) ? _prevObsId : res[1];
	    $p.find('a').html(lbl);
	    // $p.show();
	    $p.fadeIn();
	}
    } else {
	_prevObsId = undefined;
	// $p.hide();
	$p.fadeOut();
    }

    if (nextObs !== null) {
	res = _getObsIdName(nextObs);
	if (res === undefined) {
	    _nextObsId = undefined;
	    // $n.hide();
	    $n.fadeOut();
	} else {
	    _nextObsId = res[0];
	    lbl = (res[1] === undefined) ? _nextObsId : res[1];
	    $n.find('a').html(lbl);
	    // $n.show();
	    $n.fadeIn();
	}
    } else {
	_nextObsId = undefined;
	// $n.hide();
	$n.fadeOut();
    }
}

// return obj.key1.key2 or undefined.
function getKeyed(obj, key1, key2) {
    if (key1 in obj) {
	var o = obj[key1];
	if (o === null) {
	    return undefined;
	}
	if (key2 in o) {
	    return o[key2];
	} else {
	    return undefined;
	}
    } else {
	return undefined;
    }
}

// Returns the value of ObservedTime or ApprovedTime
// or undefined.
//
function getScienceExposure(val) {

    var texp = getKeyed(val, "ObservedTime", "S");
    return (texp === undefined) ? 
	getKeyed(val, "ApprovedTime", "S") : texp;
}

// Given a science obs or non-science obs, 
// return the start and end times (in miliseconds),
// and an integer value, otherwise undefined.
//
// The integer value is -1 if the observation has
// finished, 0 if it is running, and 1 if it is in
// the future.
function getStartEndTimes(val) {
    var start = getKeyed(val, "StartTime", "UTCTime");
    if (start === undefined) {
	return undefined;
    }
    start = new Date(start);
    if (start === undefined) {
	console.log("ERROR: getStartEndTimes unable to convert starttime=" + start);
	return undefined;
    }
    var texp = getKeyed(val, "Time", "S");
    if (texp === undefined) {
	texp = getScienceExposure(val);
    }
    if (texp === undefined) {
	console.log("ERROR: getStartEndTimes unable to find end time");
	return undefined;
    }
    // duration is in ks, need to convert to ms
    texp = texp * 1000 * 1000;
    start = start.getTime();
    var end = start + texp;
    var now = Date.now();
    var flag;
    if (end < now) {
	flag = -1;
    } else if (start > now) {
	flag = 1;
    } else {
	flag = 0;
    }
    return [start, end, flag];
}

// Equivalent of Hsakell's divMod, at least for positive
// values. Assumes x and y are integers.
function divMod(x, y) {
    var a = Math.floor(x / y);
    var b = x % y;
    return [a, b];
}

function _units(val, unit) {
    if (val == 0) {
	return "";
    } else if (val == 1) {
	return "1 " + unit;
    } else {
	return val + " " + unit + "s";
    }
}

// Make a "nice" readable value, i.e. 
//   "x unit1 y unit2"
//
// This is a simple conversion of the Haskell code
// into JavaScript, rather than trying to write it
// in JS from scratch.
//
// The scale value is assumed to be an integer.
//
function showUnits(v, scale, unit1, unit2) {
    var v1 = Math.ceil(v);
    var res, a, b;
    res = divMod(v1, scale);
    a = res[0];
    b = res[1];
 
    var astr = _units(a, unit1);
    var bstr = _units(b, unit2);
    var sep;
    if ((astr === "") || (bstr === "")) {
	sep = "";
    } else {
	sep = " and ";
    }

    return astr + sep + bstr;
}

// Convert a vlaue in ks into a "human-readable" value
function showExpTime(tks) {
    var h, m, s;
    s = tks * 1000;
    m = s / 60.0;
    h = m / 60.0;

    if (s < 3600) {
	return showUnits(s, 60, "minute", "second");
    } else if (h < 24) {
	return showUnits(m, 60, "hour", "minute");
    } else {
	return showUnits(h, 24, "day", "hour");
    }
}

function _plural(v) {
    if (v > 1) { return "s"; } else { return ""; }
}

var _monthnames = 
    ['January', 'February', 'March', 'April', 'May',
     'June', 'July', 'August', 'September',
     'October', 'November', 'December'];

var _daynames =
    ['Sunday', 'Monday', 'Tuesday', 'Wednesday',
     'Thursday', 'Friday', 'Saturday'];

// Would like the equivalent of Haskell's
// '%A, %B %e, %Y' format, that is
// <day of the week>, <month name> <day in month>, <year> 
// The Haskell version uses UTC, but here we can use
// the current time setting, but still in English.
// 
function showTime(time) {
    // return time.toLocaleDateString();
    return _daynames[time.getDay()] + ", " + 
	_monthnames[time.getMonth()] + " " +
	time.getDate() + ", " +
	time.getFullYear();
}

// Convert an integer 0 - 99 into a 0-padded string.
// There is no domain checking, so values outside this
// range are going to produce strange results.
function numberify(v) {
    if (v < 10) { return "0" + v; }
    else        { return v; }
}

// Convert a number into %04.f format
//
// not very pretty/robust.
function floatify(v) {
    var vint = Math.floor(v);
    var vfloat = v - vint;

    return numberify(vint) +
	vfloat.toFixed(1).slice(1);
}

// assume v >= 0
function properFraction(v) {
    var a = Math.floor(v);
    return [a, v-a];
}

// Convert ra, in decimal degrees, into a human-readable
// format.
//
function raStr(ra) {
    var rah, ram, h, m, s;
    var r1, r2, res;
    rah = ra / 15.0;
    res = properFraction(rah);
    h = res[0];
    r1 = res[1];
    ram = r1 * 60;
    res = properFraction(ram);
    m = res[0];
    r2 = res[1];
    s = r2 * 60;

    return numberify(h) + "<sup>h</sup> " +
	numberify(m) + "<sup>m</sup> " +
	floatify(s) + "<sup>s</sup>";
}

// Convert dec, in decimal degrees, into a human-readable
// format.
//
function decStr(dec) {
    var dabs;
    var d, m, s, dm;
    var res;

    dabs = Math.abs(dec);
    res = properFraction(dabs);
    d = res[0];
    dm = res[1] * 60;
    res = properFraction(dm);
    m = res[0];
    s = res[1] * 60;
    
    var decStr;
    if (dec < 0) {
	decStr = "-";
    } else {
	decStr = "+";
    }

    decStr += numberify(d) + "\u00b0 " +
	numberify(m) + "' " +
	floatify(s) + '"';

    return decStr;
}

// Convert from short- to long-form for the
// Constellation name.
var _constellationNames = {
  "And": "Andromeda"
  , "Ant": "Antlia"
  , "Aps": "Apus"
  , "Aqr": "Aquarius"
  , "Aql": "Aquila"
  , "Ara": "Ara"
  , "Ari": "Aries"
  , "Aur": "Auriga"
  , "Boo": "Boötes"
  , "Cae": "Caelum"
  , "Cam": "Camelopardalis"
  , "Cnc": "Cancer"
  , "CVn": "Canes Venatici"
  , "CMa": "Canis Major"
  , "CMi": "Canis Minor"
  , "Cap": "Capricornus"
  , "Car": "Carina"
  , "Cas": "Cassiopeia"
  , "Cen": "Centaurus"
  , "Cep": "Cepheus"
  , "Cet": "Cetus"
  , "Cha": "Chamaeleon"
  , "Cir": "Circinus"
  , "Col": "Columba"
  , "Com": "Coma Berenices"
  , "CrA": "Corona Austrina"
  , "CrB": "Corona Borealis"
  , "Crv": "Corvus"
  , "Crt": "Crater"
  , "Cru": "Crux"
  , "Cyg": "Cygnus"
  , "Del": "Delphinus"
  , "Dor": "Dorado"
  , "Dra": "Draco"
  , "Equ": "Equulus"
  , "Eri": "Eridanus"
  , "For": "Fornax"
  , "Gem": "Gemini"
  , "Gru": "Grus"
  , "Her": "Hercules"
  , "Hor": "Horologium"
  , "Hya": "Hydra"
  , "Hyi": "Hydrus"
  , "Ind": "Indus"
  , "Lac": "Lacerta"
  , "Leo": "Leo"
  , "LMi": "Leo Minor"
  , "Lep": "Lepus"
  , "Lib": "Libra"
  , "Lup": "Lupus"
  , "Lyn": "Lynx"
  , "Lyr": "Lyra"
  , "Men": "Mensa"
  , "Mic": "Microscopium"
  , "Mon": "Monoceros"
  , "Mus": "Musca"
  , "Nor": "Norma"
  , "Oct": "Octans"
  , "Oph": "Ophiuchus"
  , "Ori": "Orion"
  , "Pav": "Pavo"
  , "Peg": "Pegasus"
  , "Per": "Perseus"
  , "Phe": "Phoenix"
  , "Pic": "Pictor"
  , "Psc": "Pisces"
  , "PsA": "Piscis Austrinus"
  , "Pup": "Puppis"
  , "Pyx": "Pyxis"
  , "Ret": "Reticulum"
  , "Sge": "Sagitta"
  , "Sgr": "Sagittarius"
  , "Sco": "Scorpius"
  , "Scl": "Sculptor"
  , "Sct": "Scutum"
  , "Ser": "Serpens"
  , "Sex": "Sextans"
  , "Tau": "Taurus"
  , "Tel": "Telescopium"
  , "Tri": "Triangulum"
  , "TrA": "Triangulum Australe"
  , "Tuc": "Tucana"
  , "UMa": "Ursa Major"
  , "UMi": "Ursa Minor"
  , "Vel": "Vela"
  , "Vir": "Virgo"
  , "Vol": "Volans"
  , "Vul": "Vulpecula"
};

function constellationLink(con) {
    // there should be no cases where the lookup fails, but
    // just in case.
    var name = _constellationNames[con];
    name = (name === undefined) ? con : name;
    return "<a href='/search/constellation/" + con + "'>" + name + "</a>";
}


// With t2 >= t1, return the difference in time
// between the two, in a bunch of different
// units, shown below. Note that t1 and t2 are
// date objects.
//
//    seconds
//    number of days (int)
//              hours (int)
//              minutes (int)
//    days
//    hours
//    minutes
//
function getTimeElems(t1, t2) {
    // Too laxy to look for a good-quality JavaScript time library
    // that does this for me.
    //
    var delta = (t2.getTime() - t1.getTime()) / 1000.0;
    var m = delta / 60.0;
    var h = delta / 3600.0;
    var d = delta / (24 * 3600.0);
    var nm = Math.round(m);
    var nh = Math.round(h);
    var nd = Math.round(d);
    return [delta, nd, nh, nm, d, h, m];
}

// NOTE: I am not bothered about changes in the current time
// during the rendering of the page, although perhaps I should
// for the very unlikely case of a page being rendered during
// the switch over between not-observerd and observing or
// observing and done.
//

// Return a human-readable string representing the difference
// in time between now and time (which is a number of milliseconds),
// where time is expected to be >= now.
//
function showTimeDeltaFwd(time) {
    var future = new Date(time);
    var now    = new Date();
    var res = getTimeElems(now, future);

    var delta = res[0];
    if (delta < 60) {
	return "now";
    }

    var nd = res[1];
    var nh = res[2];
    var nm = res[3];
    var d  = res[4];
    var h  = res[5];
    var m  = res[6];

    if (m < 60) {
	return "in " + nm + " minute" + _plural(nm);
    } else if (h < 24) {
	return "in " + nh + " hour" + _plural(nh);
    } else if (d < 7) {
	return "in " + nd + " day" + _plural(nd);
    } else {
	return "on " + showTime(future);
    }
}

// Return a human-readable string representing the difference
// in time between time (which is a number of milliseconds) and
// now, where time is expected to be <= now.
//
function showTimeDeltaBwd(time) {
    var past = new Date(time);
    var now  = new Date();
    var res = getTimeElems(past, now);

    var delta = res[0];
    if (delta < 60) {
	return "now";
    }

    var nd = res[1];
    var nh = res[2];
    var nm = res[3];
    var d  = res[4];
    var h  = res[5];
    var m  = res[6];

    if (m < 60) {
	return nm + " minute" + _plural(nm) + " ago";
    } else if (h < 24) {
	return nh + " hour" + _plural(nh) + " ago";
    } else if (d < 7) {
	return nd + " day" + _plural(nd) + " ago";
    } else {
	return "on " + showTime(past);
    }
}

function hideImageBlock() {
    // $( '.radiobuttons' ).hide();
    // $( '.links' ).hide();
    $( '.radiobuttons' ).fadeOut();
    $( '.links' ).fadeOut();
}

function showImageBlock() {
    // $( '.radiobuttons' ).display();
    // $( '.links' ).display();
    $( '.radiobuttons' ).fadeIn();
    $( '.links' ).fadeIn();
}

// Set up the links/images/text for the image block and show it.
//
// How do we want to handle image loading?
//
// TODO: perhaps the selection should be retained
//   where possible - i.e. select details, move
//   forward or back, still have details selected.
//
function setupImageBlock(obsdata, sobs, mprop) {

    // TODO:
    //   need to work out how to tell the user when an
    //   image is still being loaded and to delete the
    //   'old' image
    var hdr = 'http://asc.harvard.edu/targets/' + obsdata.sequence + 
	'/' + obsdata.sequence + '.' + obsdata.obsid + '.soe.';

    $( '#DSS'  ).prop('src', hdr + 'dss.gif');
    $( '#RASS' ).prop('src', hdr + 'rass.gif');
    $( '#PSPC' ).prop('src', hdr + 'pspc.gif');

    // $( '#DSS'   ).removeClass('inactive').addClass('active');
    // $( '#DSS'   ).removeClass('inactive');
    // $( '#RASS'  ).removeClass('active').addClass('inactive');
    // $( '#PSPC'  ).removeClass('active').addClass('inactive');

    // $( '#DSSbutton' ).prop('checked', true);
    // $( '#RASSbutton' ).prop('checked', false);
    // $( '#PSPCbutton' ).prop('checked', false);

    // TODO: could this instead be included on this page?
    $( '#WWTbutton' ).prop('href', '/obsid/' + obsdata.obsid + '/wwt')

    renderObsIdDetails(obsdata, sobs, mprop);
    switchOption();
}

// make a table row for renderObsIdDetails
function keyVal(l,r) {
    return "<tr><td class='key'>" + l +
	"</td><td class='value'>" + r +
	"</td></tr>";
}

function sequenceLink(obsid) {
    return "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=sequenceSummary&obsid=" + obsid;
}

function instLinkSearch(inst) {
    return "<a href='/search/instrument/" + inst + "'>" + inst + "</a>";
}

// display the observation details as a table
//
function renderObsIdDetails(obsdata, sobs, mprop) {

    var $tbldiv = $( '#Details' );
    var h = "<table><tbody>";

    h += keyVal("Target:", obsdata.target);

    h += keyVal("Observation Details:",
		obsidLink(obsdata.obsid));
    h += keyVal("Sequence Summary:",
		"<a href='" +
		sequenceLink(obsdata.obsid) +
		"'>" + obsdata.sequence + "</a>");
    h += keyVal("Proposal Id:",
		"<a href='/proposal/" + obsdata.propNum + "'>" +
		obsdata.propNum + "</a>");

    if (mprop[0] === 'Success') {
	var prop = mprop[1];
	h += keyVal('Proposal:',
		    'Cycle ' + prop['Cycle'] + ', ' +
		    prop['Type'] + ', ' +
		    "<a href='/search/category/" +
		    encodeURIComponent(prop['Category']) +
		    "'>" + prop['Category'] + "</a>"
		   );
    }

    if (sobs['TOO'] !== null) {
	h += keyVal("TOO:", sobs['TOO']);
    }

    var ih = instLinkSearch(obsdata.instrument);
    if (obsdata.grating !== "NONE") {
	ih += ", " + obsdata.grating;
    }
    h += keyVal("Instrument:", ih);

    if (sobs["Detector"] !== null) {
	h += keyVal("Chips:", sobs["Detector"]);
    } else if (obsdata.instrument.startsWith("ACIS")) {
	var chips = [[0,"I0"], [1,"I1"], [2,"I2"], [3,"I3"]
		     , [4,"S0"], [5,"S1"], [6,"S2"], [7,"S3"]
		     , [8,"S4"], [9,"S5"]].filter(function(d) {
			 var v = sobs["ACIS" + d[1]];
			 return v !== "chipoff"; // include optional chips here
		     }).map(function(d) { return d[0]; });
	h += keyVal("Chips:", "ACIS-" + chips.join(""));
    }

    // maybe subArray
    
    if (sobs["DataMode"] !== null) {
	h += keyVal("Data Mode:", sobs["DataMode"]);
    }

    var tval = new Date(obsdata.startTime);
    var tstr = numberify(tval.getUTCHours()) + ":" +
	numberify(tval.getUTCMinutes()) + " " +
	_daynames[tval.getUTCDay()] + ", " +
	tval.getUTCDate() + " " +
	_monthnames[tval.getUTCMonth()] + " " +
	tval.getUTCFullYear() + " (UTC)";
    
    h += keyVal("Date:", tstr); 

    if (sobs["ObservedTime"] === null) {
	h += keyVal("Exposure (approved):",
		    sobs["ApprovedTime"]["S"] +
		    " ks");
    } else {
	h += keyVal("Exposure (observed):",
		    sobs["ObservedTime"]["S"] +
		    " ks");
    }

    h += keyVal("Right Ascension:", 
		raStr(getKeyed(sobs, "RA", "RA")));

    h += keyVal("Declination:", 
		decStr(getKeyed(sobs, "Dec", "Dec")));

    h += keyVal("Roll:", sobs["Roll"] + "\u00b0");

    var con = getKeyed(sobs, 'Constellation', 'ConShort');
    h += keyVal("Constellation:", constellationLink(con));

    var jobs = getJointObs(sobs, function(t) { return t + " ks"; });
    if ((sobs["JointWith"] !== null) && (jobs.length === 0)) {
	h += keyVal("Joint with:", cleanJointName(sobs["JointWith"]));
    } else if (jobs.length > 0) {
	h += jobs.map(function(d) { return keyVal("Joint with:", d);
				  }).join();
    }

    // This repeats some code elsewhere.
    var constraints = [["Time critical:", sobs["TimeCritical"]],
		       ["Monitoring:", sobs["Monitor"]],
		       ["Constrained:", sobs["Constrained"]]];
    constraints = constraints
           .filter(function(d) { return d[1] !== "noconstraint"; })
           .map(function(d) { 
	       var lbl = d[1];
	       if (lbl === 'preferred')    { lbl = "Preferred"; }
	       else if (lbl == 'required') { lbl = "Yes"; }
	       return keyVal(d[0], lbl);
	   });
    h += constraints.join("");

    h += "</tbody></table>";
    $tbldiv.html(h);
    // tbldiv.hide();  -- this conflicts with the class-based approach to hide/show; need to clean up
    $tbldiv.addClass('inactive');
}

function reportScienceObs($el, so) {

    // if there's any error, make sure previous contents are removed
    $el.html("");
    // hideImageBlock();

    var errResp = "<p>Science observation.</p>";

    var obsid = getKeyed(so, "ObsId", "ObsId");
    if (obsid === undefined) {
	console.log("ERROR: missing ObsId");
	$el.html(errResp);
	return;
    }

    var sequence = getKeyed(so, "Sequence", "Sequence");
    if (obsid === undefined) {
	console.log("ERROR: missing sequence for obsid=" + obsid);
	$el.html(errResp);
	return;
    }

    var target = so["Target"];
    if (target === undefined) {
	console.log("ERROR: missing target name for obsid=" + obsid);
	$el.html(errResp);
	return;
    }
  
    var propNum = getKeyed(so, 'Proposal', 'PropNum');
    if (propNum === undefined) {
	console.log("ERROR: Unable to get proposal# for obsid=" + obsid);
	$el.html(errResp);
	return;
    }
    var rsp = getStartEndTimes(so);
    if (rsp === undefined) {
	console.log("ERROR: Unable to get start/end times for obsid=" + obsid);
	$el.html(errResp);
	return;
    }

    var startTime, endTime, obsFlag;
    startTime = rsp[0];
    endTime   = rsp[1];
    obsFlag   = rsp[2];

    var obsLen = getScienceExposure(so);
    if (obsLen === undefined) {
	console.log("ERROR: unable to find observation length for obsid=" + obsid);
	$el.html(errResp);
	return;
    }
    obsLen = showExpTime(obsLen);

    var instrument = so["Instrument"];
    if (instrument === undefined) {
	console.log("ERROR: unable to find instrument for obsid=" + obsid);
	$el.html(errResp);
	return;
    }
    instrument = _toInstrument(instrument);
    var grating = _toGrating(so["Grating"]);
    if (grating === undefined) {
	console.log("ERROR: unable to find grating for obsid=" + obsid);
	$el.html(errResp);
	return;
    }
    grating = _toGrating(grating);

    var obsdata = { obsid: obsid
		    , sequence: sequence
		    , target: target
		    , startTime: startTime
		    , endTime: endTime
		    , obsFlag: obsFlag
		    , obsLen: obsLen
		    , propNum: propNum
		    , instrument: instrument
		    , grating: grating
		  };

    // Get the simbad, proposal information, and related obs.
    //
    $.when(
	$.ajax({
	    url: "/api/simbad/name/" + encodeURIComponent(target),
	    dataType: "json"
	}),
	$.ajax({
	    url: "/api/proposal/" + propNum,
	    dataType: "json"
	}),
	$.ajax({
	    url: "/api/related/" + propNum + "/" + obsid,
	    dataType: "json"
	})
    ).done(function(simrsp,proprsp,relrsp) {
	renderScienceObs($el, so, obsdata,
			 simrsp, proprsp, relrsp);
    });

} // reportScienceObs

// Are the two names similar enough as to not warrant
// displaying both?
//
function similarName(simbadName, targetName) {
    var n1 = simbadName.replace(/ /g, '').toLowerCase();
    var n2 = targetName.replace(/ /g, '').toLowerCase();
    return (n1 === n2);
}


// Setup the click handler for the element link
// to display the given obsid.
//
function changeObsId(obsid) {
    $.ajax({
	url: "/api/obsid/" + obsid,
	dataType: "json"
    }).done(renderObsInfo);
}

// maybe just give these a class, so that we can add a handler to them all,
// and it extracts the obsid from the text and uses that in the handler call?
function obsidLink(obsid) {
    return "<a href='#' onclick='changeObsId(" + obsid + ");'>" + obsid + "</a>";
}

// Given a set of related observations, group by target name.
function groupProposal(target, matches) {

    // find out what data we have, and then group them together
    var obs = matches.map(function(d) { return [d["Target"], d["ObsId"]["ObsId"]]; });
    var grps = _.groupBy(obs, function(d) { return d[0]; });

    // Special case when there is only one target
    if ((_.keys(grps).length === 1) && (_.values(grps)[0][0][0] === target)) {
	return _.values(grps)[0].map(function(val,key) { return obsidLink(val[1]); }).join(", ");
    } else {

	var out = _.chain(grps)
	            .pairs()
	            .map(function(d) {
			return d[0] + " (" +
			    d[1].map(function(dd) {
				return obsidLink(dd[1]);
			    }).join(", ") + ")";
		    })
                    .join("; ")
                    .value();

	return out;
    }
}

// With the change to the handling of the ajax calls,
// what was proprsp[0]/proprsp[1] is now proprsp[0][0]/proprsp[0][1]
// and similar for simrsp/relrsp
//
// TODO: el -> $el
function renderScienceObs($el, so, obsdata, simrsp, proprsp, relrsp) {

    // TODO: why did this change?
    simrsp  = simrsp[0];
    proprsp = proprsp[0];
    relrsp  = relrsp[0];
    setupImageBlock(obsdata, so, proprsp);

    var hasSimbad = simrsp[0] === 'Success';
    var hasProp   = proprsp[0] === 'Success';

    var otherName = "";
    if (hasSimbad) {
	var simbadName = simrsp[1]["Name"];
	if (!similarName(simbadName, obsdata.target)) {
	    // TODO: use &em;
	    otherName = "- also called " + simbadName + " -";
	}
    }
  
    var verb, verb2;
    if (obsdata.obsFlag < 0) {
	verb = "was";
	verb2 = "were";
    } else if (obsdata.obsFlag > 0) {
	verb = "will be";
	verb2 = "will all be";
    } else {
	verb = "is";
	verb2 = "will be";
    }

    var instInfo = "by " + instLinkSearch(obsdata.instrument);
    if (obsdata.grating !== "NONE") {
	instInfo += " and the " + obsdata.grating;
    }

    var h = "<p>The target - " + obsdata.target;
    if (obsdata.obsFlag < 0) {
	// done
	h += " - was observed " + instInfo + " for " + obsdata.obsLen;
	h += ", ended " + showTimeDeltaBwd(obsdata.endTime);
    } else if (obsdata.obsFlag > 0) {
	// to do
	h += " - will be observed " + instInfo + " for " + obsdata.obsLen;
	h += ". It will start " + showTimeDeltaFwd(obsdata.startTime);
    } else {
	// doing
	h += " - is being observed " + instInfo + " for " + obsdata.obsLen;
	h += ". The observation started " +
	    showTimeDeltaBwd(obsdata.startTime) + " and ends " +
	    showTimeDeltaFwd(obsdata.endTime);
    }
    
    if (hasProp) {
	var propName = proprsp[1]["Name"];
	h += ", and is part of the proposal <a href='/proposal/" + obsdata.propNum + "'>" +
	    propName + "</a>";
	if (!propName.endsWith('.') && !propName.endsWith('?')) {
	    h += ".";
	}
    } else {
	h += ". See why it <a href='http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=propAbstract&amp;obsid=" +
	    obsdata.obsid + "'>";
	if (obsdata.obsFlag < 0) {
	    h += "was";
	} else if (obsdata.obsFlag > 0) {
	    h += "will be";
	} else {
	    h += "is being";
	}
	h += " observed</a>.";
    }

    h += "The target ";

    var con = getKeyed(so, 'Constellation', 'ConShort');
    if (con !== undefined) {
	h += otherName + " is located in the constellation " +
	    constellationLink(con);
	if (hasSimbad) { h += " and "; }
    }

    var subArrayTxt = "";
    var subArrayStart = so["SubArrayStart"];
    var subArraySize = so["SubArraySize"];
    if ((obsdata.grating === "NONE") &&
	(subArrayStart !== null) &&
	(subArraySize !== null)) {
	var frac = Math.floor(1024 / subArraySize);
	if (frac > 1) {
	    var fracTxt;
	    if (frac == 2)      { fracTxt = "only half of the chip "; }
	    else if (frac == 4) { fracTxt = "only one-quarter of the chip "; }
	    else if (frac == 8) { fracTxt = "only one-eigth of the chip "; }
	    else                { fracTxt = "a custom sub array "; }
	    subArrayTxt = "The source is so bright in X-rays that " + fracTxt + verb +
		" used for the observation. ";
	}
    }
	    
    if (hasSimbad) {
	var simName = simrsp[1]["Name"];
	var simType = simrsp[1]["Type"];
	var simType3 = getKeyed(simrsp[1], "Type3", "SimbadType");

	var simLink = "Ident=" + encodeURIComponent(simName);

	var typestr = simType;
	if (typestr === "Region defined in the sky") {
	    typestr = "an area of the sky";
	} else if ("aeiou".contains(typestr[0].toLowerCase())) {
	    typestr = "an " + typestr;
	} else {
	    typestr = "a " + typestr;
	}

	h += " is <a href='/search/type/" + encodeURIComponent(simType3) +
	    "'>" + typestr + "</a>. " + subArrayTxt +
	    "More information on the target can be found at " +
	    "<a href='http://simbad.harvard.edu/simbad/sim-id?" + simLink + "'>SIMBAD</a>.";
    } else {
	h += ". " + subArrayTxt;
    }

    if ((relrsp[0] === 'Success') && (relrsp[1].length > 0)) {
	var matches = relrsp[1];
	var suffix = "";
	if (matches.length > 1) {
	    suffix = "s";
	}
	h += " See related observation" + suffix + ": " +
	    groupProposal(obsdata.target, matches) + ".";
    }

    h += "</p>";

    var jName = so["JointWith"];
    if (jName !== null) {
	h += "<p>";
	h += "This " + verb + " a joint observation with ";
	var jobs = getJointObs(so, showExpTime);
	if (jobs.length === 0) {
	    h += cleanJointName(jName);
	} else {
	    h += addList(jobs);
	}
	h += ". However, it does not necessarily mean that the observations " +
	    verb2 + " done at the same time!</p>";
    }

    var constraints = [["time critical", so["TimeCritical"]],
		       ["monitoring", so["Monitor"]],
		       ["constrained", so["Constrained"]]];
    constraints = constraints.filter(function(d) { return d[1] !== "noconstraint"; });
    constraints = constraints.map(function(d) { 
	if (d[1] === "preferred") { return d[0] + " (preferred)"; }
	else                      { return d[0]; }
    });
    var hasConstraint = constraints.length > 0;
    if (hasConstraint) {
	if (jName === null) { h += "<p>"; }
	
	h += "This ";
	if (obsdata.obsFlag < 0) { h += "was"; }
	else             { h += "is"; }
	h += " a ";
	h += addList(constraints);
	h += " observation.";
    }

    if ((jName !== null) || hasConstraint) { h += "</p>"; }

    if (so["TOO"] !== null) {
	h += "<p>This ";
	if (obsdata.obsFlag < 0) { h += "was"; }
	else             { h += "is"; }
	h += " a TOO (target of opportunity) observation.</p>";
    }

    $el.html(h);
}

// You can get a 'will start now' situation (maybe only for
// those with no obs length?). This happens for both the
// Haskell and JavaScript versions.
//
function reportNonScienceObs($el, nso) {
    var errResp = "<p>Calibration observation.</p>";

    // hide the image links
    // $( '.radiobuttons' ).hide();
    // $( '.links' ).hide();
    $( '.radiobuttons' ).fadeOut();
    $( '.links' ).fadeOut();

    var rsp = getStartEndTimes(nso);
    if (rsp === undefined) {
	$el.html(errResp);
	return;
    }

    var target = nso["Target"];
    if (target === undefined) {
	$el.html(errResp);
	return;
    }
    
    var startTime, endTime, obsFlag;
    startTime = rsp[0];
    endTime   = rsp[1];
    obsFlag   = rsp[2];

    var obsLen = undefined;
    if (endTime > startTime) {
	obsLen = showExpTime(getKeyed(nso, "Time", "S"));
    }

    var h = "<p>The calibration observation - " + target;
    
    if (obsFlag < 0) {
	// finished
	if (obsLen === undefined) {
	    h += " - finished ";
	} else {
	    h += " - was run for " + obsLen + " and finished ";
	}
	h += showTimeDeltaBwd(endTime) + ".";
    } else if (obsFlag > 0) {
	// not done
	if (obsLen === undefined) {
	    h += " - will start ";
	} else {
	    h += " - will run for " + obsLen + ", starting ";
	}
	h += showTimeDeltaFwd(startTime) + ".";
    } else {
	// doing
	if (obsLen === undefined) {
	    h += " - is running now";
	} else {
	    h += " - is running for " + obsLen;
	}
	h += ". The observation started ";
	h += showTimeDeltaBwd(startTime) + " and ends ";
	h += showTimeDeltaFwd(endTime) + ".";
    }

    $el.html(h);
}

// Display the observation.
//
// Trying to work out the best fadeIn/Out versus
// straight replacement. At present it's likely that
// the replacement is happening quick enough that
// it occurs before the fade out works. The current
// CSS layout is "sub-optimal" for this.
//
function renderObsInfo(rsp) {
    var status = rsp[0];
    var answer = rsp[1];
    var $el = $( '#observation' );

    hideImageBlock();

    if (status === "Success") {
	renderNavBar(answer);

	var co = answer["CurrentObs"];
	if ("Right" in co) {
	    $el.fadeOut();
	    reportScienceObs($el, co["Right"]);
	    showImageBlock();
	    $el.fadeIn();
	} else if ("Left" in co) {
	    reportNonScienceObs($el, co["Left"]);
	} else {
	    $el.html("<p>Did not understand the response!</p>");
	}
    } else {
	$el.html("Failed: " + answer);
    }
}

// Given an obsid, render the page.
function renderObsId(rsp) {
    var status = rsp[0];
    if (status === "Failed") {
	$( "#observation" ).html("Error: no observation is found...");
	return;
    }
    var obsid = rsp[1];
    $.ajax({
	url: "/api/obsid/" + obsid,
	dataType: "json"
    }).done(renderObsInfo);
}

function renderCurrent() {
    // TODO: the current obs should be updated
    //       periodically
    $.ajax({
	url: "/api/current",
	dataType: "json"
    }).done(function (rsp) {
	if (rsp[0] == "Success") {
	    _currObsId = rsp[1];
	}
	renderObsId(rsp);
    });
}

function initialize() {

    setupCurrentClick();
    setupNavBarOnClick();

    renderCurrent();
}

var hack;

// end
