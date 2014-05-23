# Accessing and storing the Chandra Short-Term Schedule

The aim is to periodically poll the Chandra Short-Term schedule
- at present a manual extraction of the information presented
on <http://cxc.harvard.edu/target_lists/stscheds/index.html>
but eventually using the 
[commanded-states database](http://cxc.cfa.harvard.edu/mta/ASPECT/tool_doc/cmd_states/).

These updates will have to be sent to the application/web server,
which implies that [acid-state](http://acid-state.seize.it) is not
a good match. There are other reasons not to use `acid-state`,
the prime one being sociological rather than technical (I have used
it before and want to use a different approach here), but also it
is not integrated into the heroku setup, unlike Postgres.

If we do use Postgres, then how do we access it? I originally
started with [persistent](http://hackage.haskell.org/package/persistent),
but it is really designed for systems where it, and it alone, is
used to create and manage the database. As access to the
commanded-states database is either HDF5 or Sybase SQL, I may
end up writing that part in a different language, so being able to
control the database schema would be preferable.

