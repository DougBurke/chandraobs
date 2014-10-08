# Thoughts on web display

Note that I am manually creating "views" of the data that
could be selectable instead via a faceted browsing interfacte.

 - the database contains scheduled observations that were
   cancelled - e.g. 16062 and 16118. If you go to 

     http://chandraobs-devel.herokuapp.com/obsid/16065

   which is the observation prior to 16062, then the page
   is displayed, but the "next observation" link is missing.
   There are no pages for 16062 or 16118 (at least at this
   time), because they have not been re-sheduled, so the ObsCat
   information has no date field, which means that the obscat
   executable skips them. What to do here? 

 - do we want any note of PI_NAME == Calibration ?

 - the links between target name and SIMBAD can be improved;
   eg drop " offset ..." when searching. This requires
   "improving" the simbad data type/database storage.

 - a view of the object types II: give some statisics on the
   object types that have been observed (broken down by time
   or AO; the latter is harder and I don't think as interesting
   to the public)

 - some form of a calendar widget to jump through the schedule;
   how about a day view (a simple condensed view of what it's
   doing), perhaps also week and/or month.

 - Can we display on sources from the CSC in WWT? The HTML5 WWT
   API has some mention of a display VoTable routine.

 - Can the WWT view display more controls/fade over between bands/
   labels/the cross hair menu thingy

 - ADS view of papers about this object/class of object/...
   by this PI

   note that the page on SIMBAD that we link to (when
   available) includes papers about the source

 - archive link to existing Chandra data

   there are links we can use like 

   http://cdaftp.harvard.edu/cgi-bin/chaser_ftp_retrieve_file.cgi?filename=byobsid/2/642/primary/acisf00642N005_full_img2.jpg

   *but* this requires that we know the processing version
   (in this case N005) for each observation.  

 - can we have a simple view showing Earth/Moon(?)/orbit and
   path during osbservation; for ongoing obs perhaps even the
   position of Chandra within the orbit and its direction?

 - a simple search box - name, prob not a cone search?
   also: view all HRC-I obs/... page?

 - in the schedule view, show the region of the sky that is
   inaccessible (roll angle + .. constraints), or even what
   part of the sky is visible now, for a given user (would
   need to know long/lat of the user to do this)

 - link to a description of what a TOO is

 - the joint-observation with field is potentially confusing,
   since I guess it's a proposal-wide field (ie should be
   considered to be a part of the proposal) rather than for
   each observation. We may want to mention this in the obsid
   view, and explicitly mention it in the proposal view.

Doing

 - working on the "api" branch that provides more information
   via JSON (although heavily Haskell flavored) and simple
   AJAX display, at /index2.html. The aim is to try moving
   to an "all in one" Haskell approach a la haste/ghcjs/...

Done

 - link to a "view by instrument" page

 - add in mention of the TOO status: for the obsid page there is
   only mention that it's a TOO, not what "turnaround" time
   was requested. Do we want to tell the readers this value (or,
   perhaps, a simpler version like "very fast", "fast",
   "pedestrian", ...)?

   this has also been added to the schedule table, as the string
   stored in ObsCat - e.g. 0-4, 4-15 - which may or may not be
   useful.

 - add in object type and proposal area to the schedule list

 - add in proposal area to the main display

 - display constellation name

 - observation abstract: have now a link to the archive version of
   the abstract, but presentation on this site needs to be
   improved, and it would be nice to provide a more-friendly
   landing page. I have an initial version up.

 - a view of the object types I: for a given object type
   display a schedule-like page showing the observations;
   this could get hairy once the database fills up.

 - information on the target (e.g. wikipedia) since I think
   NED/SIMBAD would be a bit too much. Actually, trying out
   a SIMBAD search; perhaps should include positional information
   in the search (definitely need to protect/hide characters in
   the name). For now limited to basic SIMBAD information.

 - a condensed version of the observation schedule, with some form
   of pagination
   
 - a 'tour this page' option a la the ADS 
   http://adslabs.org/adsabs/search/
     -> http://bootstraptour.com/

  