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

## Messing around

Using Groundhog to enter data into postgresql:

Used pgadmin3/psql to drop the existing tables in the database, e.g.

% psql --username=postgres --password --host=127.0.0.1 --dbname=chandraobs

chandraobs=# drop table "ScienceObs";
DROP TABLE
chandraobs=# drop table "NonScienceObs";
DROP TABLE
chandraobs=# drop table "ScheduleItem";
DROP TABLE


% ./dist/build/hackdata/hackdata
Migrating: CREATE TABLE "NonScienceObs" ("id" BIGSERIAL PRIMARY KEY UNIQUE, "nsName" VARCHAR NOT NULL, "nsObsId" INT8 NOT NULL, "nsTarget" VARCHAR NOT NULL, "nsStartTime" TIMESTAMP NOT NULL, "nsTime" DOUBLE PRECISION NOT NULL, "nsRa" DOUBLE PRECISION NOT NULL, "nsDec" DOUBLE PRECISION NOT NULL, "nsRoll" DOUBLE PRECISION NOT NULL, "nsPitch" DOUBLE PRECISION NOT NULL, "nsSlew" DOUBLE PRECISION NOT NULL)
Migrating: CREATE TABLE "ScheduleItem" ("id" BIGSERIAL PRIMARY KEY UNIQUE, "siObsName" VARCHAR NOT NULL, "siStart" TIMESTAMP NOT NULL, "siEnd" TIMESTAMP NOT NULL, "siDuration" DOUBLE PRECISION NOT NULL)
Migrating: CREATE TABLE "ScienceObs" ("id" BIGSERIAL PRIMARY KEY UNIQUE, "soSequence" INT8 NOT NULL, "soObsId" INT8 NOT NULL, "soTarget" VARCHAR NOT NULL, "soStartTime" TIMESTAMP NOT NULL, "soTime" DOUBLE PRECISION NOT NULL, "soInstrument" VARCHAR NOT NULL, "soGrating" VARCHAR NOT NULL, "soRa" DOUBLE PRECISION NOT NULL, "soDec" DOUBLE PRECISION NOT NULL, "soRoll" DOUBLE PRECISION NOT NULL, "soPitch" DOUBLE PRECISION NOT NULL, "soSlew" DOUBLE PRECISION NOT NULL)
Inserting schedule
Inserting science obs
Inserting non-science obs

% psql --username=postgres --password --host=127.0.0.1 --dbname=chandraobs

chandraobs=# select count(*) from "ScheduleItem";
 count 
-------
    92
(1 row)

chandraobs=# select count(*) from "ScienceObs";
 count 
-------
    45
(1 row)

chandraobs=# select count(*) from "NonScienceObs";
 count 
-------
    47
(1 row)

chandraobs=# \q

