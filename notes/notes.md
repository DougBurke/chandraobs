
## Chandra Short-Term Schedule

http://cxc.harvard.edu/target_lists/stscheds/index.html

which is /proj/web-cxc-dmz/htdocs/target_lists/stscheds/

There are index.html and a lot of old pages.

## Step 1

Convert index.html to a simple format, maybe JSON. The idea is to 
the feed it into postgresql.

The index.html file contains

<!-- START WEEKS -->

<!--#include virtual="stschedMAY1214B.html"--><!--thisweek-->
<!--#include virtual="stschedMAY0514A.html"--><!--lastweek-->

<!-- END WEEKS -->

Actually, have just manually converted a few observations for initial
development and then need to see about getting access to the
more-accurate-that-the-STS-page archive.

