/*
 * Tour using http://bootstraptour.com
 */

var tour = new Tour({
  /* debug: true, */
  storage: false,
  orphan: true,
  onEnd: function (tr) { tr.setCurrentStep(0); },
  steps: [
  {
    element: "#tour",
    placement: "bottom",
    title: "Welcome to the 'What is Chandra doing now?' Site",
    content: "This tour will briefly highlight the main parts of this page."
  },
  {
    element: "#observation",
    title: "What is Chandra doing now?",
    content: "Here we describe the current status of Chandra."
  },
  {
    element: "#observation",
    title: "Observations",
    content: "If this is a science observation then information on the target will be displayed; only the duration of calibration observations is given."
  },
  {
    element: ".radiobuttons",
    title: "Science Observations",
    content: "For science observations, views of the object are given in optical (DSS and with the World Wide Telescope) and X-ray (from ROSAT)."
  },
  {
    element: "#obslinks",
    placement: "bottom",
    title: "Past and Future observations",
    content: "You can look at past and future observations."
  },
  {
    element: "#twitter-widget-0", // need to update if Twitter changes its naming scheme
    placement: "top", // TODO: change
    title: "Twitter",
    content: "Tweets from the Chandra twitter account, @chandraxray."
  },
  {
    element: "#home",
    placement: "bottom",
    title: "Current observation",
    content: "This link will take you to the current observation."
  },
  {
    element: "#sched",
    placement: "bottom",
    title: "Schedule",
    content: "The Chandra schedule (several days in the past and planned observations)."
  },
  {
    element: "#views",
    placement: "bottom",
    title: "Views",
    content: "Background information on the DSS/RASS/PSPC/WWT views of the target."
  },
  {
    element: "#insts",
    placement: "left",
    title: "Instruments",
    content: "A description of the instruments on board Chandra."
  },
  {
    element: "#about",
    placement: "left",
    title: "About",
    content: "Information on this web site."
  }
]});

// Create the link to the tour
// Now we have jQuery this is easy
function addTour() {
  $( 'nav[role="navigation"] ul' ).append("<li><a id='tour' href='#' onclick='tour.start(true);'>Tour this Page</a></li>");
}

// Initialize the tour
tour.init();

