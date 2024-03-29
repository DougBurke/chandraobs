/*
 * Tour using http://bootstraptour.com
 */

"use strict";

var tour = new Tour({
  /* debug: true, */
  storage: false,
  orphan: true,
  onEnd: (tr) => { tr.setCurrentStep(0); },
  steps: [
  {
    element: "#tour",
    placement: "bottom",
    title: "Welcome to the 'What is Chandra doing now?' Site",
    content: "This tour will briefly highlight the main parts of this page."
  },
      // I think the two #observation should be combined, but
      // using two paragraphs.
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
    content: `For science observations, the object is shown in the optical (from the Digital Sky Survey) and using the World Wide Telescope (the 'Interactive' tab), and also in the X-ray (these are labelled RASS and PSPC and are from an earlier telescope called ROSAT).
<br/><br/>
The <strong>Browse interface</strong> (which you are about to hear about) is an enhanced version of the World Wide Telescope view!`
      /* " If the observation is publically available then the Chandra data will be shown by default (unfortunately it isn't always as visually impressive as data from other telescopes, such as the Hubble Space Telescope :-)." */
  },
  {
    element: "#obslinks",
    placement: "bottom",
    title: "Past and Future observations",
    content: "You can look at past and future observations."
  },
  {
    element: "#home",
    placement: "bottom",
    title: "Current observation",
    content: "This link will take you to the current observation."
  },
  {
    element: "#brows",
    placement: "bottom",
    title: "Browse",
    content: "View the information on the sky, using the World Wide Telescope. It provides similar information to this page but in a <em>much</em> snazzier way."
  },
  {
    element: "#sched",
    placement: "bottom",
    title: "Schedule",
    content: "The Chandra schedule (several days in the past and planned observations)."
  },
  {
    element: "#expl",
    placement: "bottom",
    title: "Explore",
    content: "A variety of ways to search the Chandra schedule, and find out more about the objects it observes."
  },
  {
    element: "#tour",
    placement: "bottom",
    title: "A quick tour of the site",
    content: "Display this tour ;-)"
  },
  {
    element: "#views",
    placement: "bottom",
    title: "Views",
    content: "Background information on the views of the observation: what the DSS, RASS, and PSPC are and what they mean."
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
    content: "Information on this web site, including when the database was last updated and what recent changes have been made to the website."
  }
]});

// Create the link to the tour
// Now we have jQuery this is easy
function addTour() {
  $( 'nav[role="navigation"] ul' ).append("<li><a id='tour' href='#' onclick='tour.start(true);'>Tour this Page</a></li>");
}

// Initialize the tour
tour.init();

