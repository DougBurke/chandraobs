# Thoughts on web display

Note that I am manually creating "views" of the data that
could be selectable instead via a faceted browsing interface.
For a general-public view, I think this is a sensible choice
(provide nice top-level views), but is there a place for
some form of faceted browser?

 - The "per object" page - e.g. /search/name?target=A1795
   should include a link to the SIMBAD page.

 - can the SIMBAD links be made to switch between CfA and
   CDS, as I am finding the CfA version rather flaky at the
   moment? It could be something clever like: determine from
   the users IP address which is nearer and use that (does not
   help when one is down), periodically ping and choose
   that one, ping whenever generating a page and use that,
   or let the user choose.

   All are quite invasive.
   
 - Can I improve cacheing by using ETag/LastModified? The idea
   would be to use the git commit hash (generated at build time)
   together with the last-modified-date of the database (would
   need to be added) together with the URI to create a unique
   id for a page.

   The /search/mappings/ and /api/mappings/ URIs are a good test
   case. I have managed to do something to improve the access
   to /api/mappings/, but it's not clear it's the best way.

 - Allow zoom and pan of the all-sky view, in a similar manner
   to the SkyMap view from Chandra. It would also be nice
   to switch between ecliptic and galactic coordinates.
   
 - is it worth adding views of cycle and the various constrained
   observations, as well as the TOO field?

   Would these be standard schedule views, or something different
   (e.g. a "facet" for the calendar view for cycle)?

   Perhaps highlight those days in the calendar view that
   have constraints. Or in the timeline view. Do we need to
   break down the various types of constraint? Do I have enough
   information to do this reliably?

   Done: TOO and constrained fields

 - now that I list exposure time for each constellation at
   /search/constellation/, I wonder if it would make sense
   to create a sky map color-coded by exposure time.

   There are issues with the current display over what is
   the correct inside for a polygon on a sphere.
   
 - map Chandra proposal abstracts to the Unified Astronomy
   Thesaurus, so that can use this as a selector, as is
   currently done with SIMBAD types and proposal categories.
   This likely requires paying for space on heroku, since
   the database will get significantly larger.

 - links to views of the actual data, or papers that reference
   the data. As well as database-size concerns for the test
   server I'm running, it's not obviously clear how to
   best visualize this. This is spelt out a bit more in the
   next two points.

 - ADS view of papers about this object/class of object/...
   by this PI

   note that the page on SIMBAD that we link to (when
   available) includes papers about the source

 - archive link to existing Chandra data

   there are links we can use like 

   http://cdaftp.harvard.edu/cgi-bin/chaser_ftp_retrieve_file.cgi?filename=byobsid/2/642/primary/acisf00642N005_full_img2.jpg

   *but* this requires that we know the processing version
   (in this case N005) for each observation.  

   The query
   
   http://cda.cfa.harvard.edu/chaser/viewerImageContents.do?imageType=loresimg_jpg&obsid=16275

   returns HTML which contains links to both low and hires images; in this case they
   look like

   http://cda.cfa.harvard.edu/chaser/viewerImage.do?obsid=16275&filename=acisf16275N002_full_img2.jpg&filetype=loresimg_jpg

   (so would need to parse the HTML to get this). There could be fields
   in ScienceObs that include these (with the assumption that the
   latest version in the archive is not going to change much); the
   obscat query would then include a pass to update these fields for
   observations which have gone public.

 - is it worth providing some sort of view of the exposure
   time values - e.g. provide a histogram alongside the
   schedule view? This may be more interesting for catalog
   views like "all Galaxy obervations", where there's a lot
   of sources.

   It might be nice to do a canned view of this as a function
   of time (e.g. AO or year), since the historical trends
   could be interesting (longer observations, but split up
   more)?
   
 - improve display of the joint-with fields now that there
   is /search/joint/:mission

   Include a sankey-like view linking facilities with the
   proposal category and/or SIMBAD data types. There probably
   isn't enough data to make this worthwhile (except perhaps
   for HST and NRAO).

 - do we want any note of PI_NAME == Calibration ?

 - break down into cycles? e.g. some view of what was observed
   in Cycle 16. The simple way would be essentially a facet,
   to filter on this in other views.
 
 - the links between target name and SIMBAD can be improved;
   eg drop " offset ..." when searching. This requires
   "improving" the simbad data type/database storage.

   There has been a small improvement in the rules I use,
   but more could be done.

 - a view of the object types II: give some statisics on the
   object types that have been observed (broken down by time
   or AO; the latter is harder and I don't think as interesting
   to the public)

   Done?: have /search/dtype/ and /search/mappings/
          do we need more?
   
 - Can we display on sources from the CSC in WWT? The HTML5 WWT
   API has some mention of a display VoTable routine.

 - Can the WWT view display more controls/fade over between bands/
   labels/the cross hair menu thingy

 - Add in a view of the sky using the Aladin Lite widget
   http://aladin.u-strasbg.fr/AladinLite/
 
 - can we have a simple view showing Earth/Moon(?)/orbit and
   path during osbservation; for ongoing obs perhaps even the
   position of Chandra within the orbit and its direction?

 - in the schedule view, show the region of the sky that is
   inaccessible (roll angle + .. constraints), or even what
   part of the sky is visible now, for a given user (would
   need to know long/lat of the user to do this)

 - the joint-observation with field is potentially confusing,
   since I guess it's a proposal-wide field (ie should be
   considered to be a part of the proposal) rather than for
   each observation. We may want to mention this in the obsid
   view, and explicitly mention it in the proposal view.

## Doing

 - working on the "api" branch that provides more information
   via JSON (although heavily Haskell flavored) and simple
   AJAX display, at /index2.html. The aim is to try moving
   to an "all in one" Haskell approach a la haste/ghcjs/...

   This is currently on the back burner.
   
## Done

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

 - the database contains scheduled observations that were
   cancelled - e.g. 16062 and 16118. If you go to 

     http://chandraobs-devel.herokuapp.com/obsid/16065

   which is the observation prior to 16062, then the page
   is displayed, but the "next observation" link is missing.
   There are no pages for 16062 or 16118 (at least at this
   time), because they have not been re-sheduled, so the ObsCat
   information has no date field, which means that the obscat
   executable skips them. What to do here? 

   I believe this has been addressed.

 - some form of a calendar widget to jump through the schedule;
   how about a day view (a simple condensed view of what it's
   doing), perhaps also week and/or month.

   Done: /search/calendar/

 - a simple search box - name, prob not a cone search?
   also: view all HRC-I obs/... page?

   Done: /search/index.html has a name search
         /search/instrument/ breaks down the different
             detector plus instrument views

 - when viewing a proposal, can use the jointwith field to
   determine whether this is an "outside" proposal - e.g.
   /proposal/15700897 is a HST proposal which asked for joint
   Chandra time.
   
   Done: the text on the obsid view is now slightly different
   if Chandra was not the primary source of the observation,
   and there's improve links to the mission views.

 - link to a description of what a TOO is

   DONE (although the information can be improved and may
   need to be placed elsewhere)
   
 - Can I use the matched name for an observation as the search
   label in the schedule view, so that it will group the same
   object together (at lease, once the search order is
   changed to by name)? It would likely require adding in extra
   DB queries in makeSchedule.

   I can not think of an obvious way to group the rows visually,
   given that I already use color to indicate done/doing/todo.
   It could also be distracting, since it is only useful when
   sorted by name.

   DONE: I already had the data available, so was easy. There is
   no visual indication that different names map to the same
   target.
   
## Rejected

 - The /search/dtype/ view could be switched to a Sankey
   diagram. I have tried this on the branch
   experiment-sankey-plot-for-simbad-dendogram
   and it didn't work out well.

   Also:
   
   I thought about extending /search/mappings/ to take
   advantage of the SIMBAD hierarchy, but further investigation
   suggests it does not really work: can have a link from a categoty
   to a top-level SIMBAD element, but then how to go from
   there to the children whilst stil identifying the separate
   proposal categories?

