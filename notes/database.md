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

and then fill in the data from my hand-created version:

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

## Setting up for postgresql on Heroku

    % heroku addons
    chandraobs-devel has no add-ons.
    % heroku pg:info
    chandraobs-devel has no heroku-postgresql databases.

To add the hobby-dev database (free, 10000 row limit)

    % heroku addons:add heroku-postgresql
    Adding heroku-postgresql on chandraobs-devel... done, v57 (free)
    Attached as HEROKU_POSTGRESQL_ROSE_URL
    Database has been created and is available
     ! This database is empty. If upgrading, you can transfer
     ! data from another database with pgbackups:restore.
    Use `heroku addons:docs heroku-postgresql` to view documentation.

    % heroku config
    === chandraobs-devel Config Vars
    BUILDPACK_URL:              https://github.com/begriffs/heroku-buildpack-ghc.git
    DATABASE_URL:               postgres://blah blah blah
    HEROKU_POSTGRESQL_ROSE_URL: postgres://blah blah blah

Not needed for this single-database setup:

    % heroku pg:promote HEROKU_POSTGRESQL_ROSE_URL
    Promoting HEROKU_POSTGRESQL_ROSE_URL (DATABASE_URL) to DATABASE_URL... done

    %  heroku pg:info
    === HEROKU_POSTGRESQL_ROSE_URL (DATABASE_URL)
    Plan:        Hobby-dev
    Status:      Available
    Connections: 0
    PG Version:  9.3.3
    Created:     2014-06-06 17:40 UTC
    Data Size:   6.5 MB
    Tables:      0
    Rows:        0/10000 (In compliance)
    Fork/Follow: Unsupported
    Rollback:    Unsupported

## Push data to the database:

Note that, even with setting the env vars, the password is still prompted
for (twice):

    % PGUSER=postgres PGPASSWORD=postgres heroku pg:push chandraobs HEROKU_POSTGRESQL_ROSE --app chandraobs-devel
    Password: 
    pg_dump: reading schemas
    pg_dump: reading user-defined tables
    pg_dump: reading extensions
    pg_dump: reading user-defined functions
    pg_dump: reading user-defined types
    pg_dump: reading procedural languages
    pg_dump: reading user-defined aggregate functions
    pg_dump: reading user-defined operators
    pg_dump: reading user-defined operator classes
    pg_dump: reading user-defined operator families
    pg_dump: reading user-defined text search parsers
    pg_dump: reading user-defined text search templates
    pg_dump: reading user-defined text search dictionaries
    pg_dump: reading user-defined text search configurations
    pg_dump: reading user-defined foreign-data wrappers
    pg_dump: reading user-defined foreign servers
    pg_dump: reading default privileges
    pg_dump: reading user-defined collations
    pg_dump: reading user-defined conversions
    pg_dump: reading type casts
    pg_dump: reading table inheritance information
    pg_dump: reading event triggers
    pg_dump: finding extension members
    pg_dump: finding inheritance relationships
    pg_dump: reading column info for interesting tables
    pg_dump: finding the columns and types of table "NonScienceObs"
    pg_dump: finding default expressions of table "NonScienceObs"
    pg_dump: finding the columns and types of table "Proposal"
    pg_dump: finding default expressions of table "Proposal"
    pg_dump: finding the columns and types of table "ScheduleItem"
    pg_dump: finding default expressions of table "ScheduleItem"
    pg_dump: finding the columns and types of table "ScienceObs"
    pg_dump: finding default expressions of table "ScienceObs"
    pg_dump: flagging inherited columns in subtables
    pg_dump: reading indexes
    pg_dump: reading indexes for table "NonScienceObs"
    pg_dump: reading indexes for table "Proposal"
    pg_dump: reading indexes for table "ScheduleItem"
    pg_dump: reading indexes for table "ScienceObs"
    pg_dump: reading constraints
    pg_dump: reading triggers
    pg_dump: reading rewrite rules
    pg_dump: reading large objects
    pg_dump: reading dependency data
    pg_dump: saving encoding = UTF8
    pg_dump: saving standard_conforming_strings = on
    pg_dump: saving database definition
    pg_dump: dumping contents of table NonScienceObs
    pg_dump: dumping contents of table Proposal
    pg_restore: connecting to database for restore
    pg_dump: dumping contents of table ScheduleItem
    pg_dump: dumping contents of table ScienceObs
    pg_restore: creating SCHEMA public
    pg_restore: creating COMMENT SCHEMA public
    pg_restore: creating EXTENSION plpgsql
    pg_restore: creating COMMENT EXTENSION plpgsql
    pg_restore: [archiver (db)] Error while PROCESSING TOC:
    pg_restore: [archiver (db)] Error from TOC entry 2026; 0 0 COMMENT EXTENSION plpgsql 
    pg_restore: [archiver (db)] could not execute query: ERROR:  must be owner of extension plpgsql
        Command was: COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';

    pg_restore: creating EXTENSION adminpack
    pg_restore: [archiver (db)] Error from TOC entry 178; 3079 16394 EXTENSION adminpack 
    pg_restore: [archiver (db)] could not execute query: ERROR:  permission denied to create extension "adminpack"
    HINT:  Must be superuser to create this extension.
        Command was: CREATE EXTENSION IF NOT EXISTS adminpack WITH SCHEMA pg_catalog;

    pg_restore: creating COMMENT EXTENSION adminpack
    pg_restore: [archiver (db)] Error from TOC entry 2027; 0 0 COMMENT EXTENSION adminpack 
    pg_restore: [archiver (db)] could not execute query: ERROR:  extension "adminpack" does not exist
        Command was: COMMENT ON EXTENSION adminpack IS 'administrative functions for PostgreSQL';

    pg_restore: creating FUNCTION ScienceObs#soJointWith()
    pg_restore: creating FUNCTION ScienceObsFull()
    pg_restore: creating FUNCTION ScienceObsFull#sofJointWith()
    pg_restore: creating TABLE NonScienceObs
    pg_restore: creating SEQUENCE NonScienceObs_id_seq
    pg_restore: creating SEQUENCE OWNED BY NonScienceObs_id_seq
    pg_restore: creating TABLE Proposal
    pg_restore: creating SEQUENCE Proposal_id_seq
    pg_restore: creating SEQUENCE OWNED BY Proposal_id_seq
    pg_restore: creating TABLE ScheduleItem
    pg_restore: creating SEQUENCE ScheduleItem_id_seq
    pg_restore: creating SEQUENCE OWNED BY ScheduleItem_id_seq
    pg_restore: creating TABLE ScienceObs
    pg_restore: creating SEQUENCE ScienceObs_id_seq
    pg_restore: creating SEQUENCE OWNED BY ScienceObs_id_seq
    pg_restore: creating DEFAULT id
    pg_restore: creating DEFAULT id
    pg_restore: creating DEFAULT id
    pg_restore: creating DEFAULT id
    pg_restore: processing data for table "NonScienceObs"
    pg_restore: executing SEQUENCE SET NonScienceObs_id_seq
    pg_restore: processing data for table "Proposal"
    pg_restore: executing SEQUENCE SET Proposal_id_seq
    pg_restore: processing data for table "ScheduleItem"
    pg_restore: executing SEQUENCE SET ScheduleItem_id_seq
    pg_restore: processing data for table "ScienceObs"
    pg_restore: executing SEQUENCE SET ScienceObs_id_seq
    pg_restore: creating CONSTRAINT NonScienceObsIdConstraint
    pg_restore: creating CONSTRAINT NonScienceObs_pkey
    pg_restore: creating CONSTRAINT PropConstraint
    pg_restore: creating CONSTRAINT Proposal_pkey
    pg_restore: creating CONSTRAINT ScheduleItem_pkey
    pg_restore: creating CONSTRAINT ScheduleitemObsIdConstraint
    pg_restore: creating CONSTRAINT ScienceObsIdConstraint
    pg_restore: creating CONSTRAINT ScienceObs_pkey
    pg_restore: setting owner and privileges for DATABASE chandraobs
    pg_restore: setting owner and privileges for SCHEMA public
    pg_restore: setting owner and privileges for COMMENT SCHEMA public
    pg_restore: setting owner and privileges for EXTENSION plpgsql
    pg_restore: setting owner and privileges for COMMENT EXTENSION plpgsql
    pg_restore: setting owner and privileges for EXTENSION adminpack
    pg_restore: setting owner and privileges for COMMENT EXTENSION adminpack
    pg_restore: setting owner and privileges for FUNCTION ScienceObs#soJointWith()
    pg_restore: setting owner and privileges for FUNCTION ScienceObsFull()
    pg_restore: setting owner and privileges for FUNCTION ScienceObsFull#sofJointWith()
    pg_restore: setting owner and privileges for TABLE NonScienceObs
    pg_restore: setting owner and privileges for SEQUENCE NonScienceObs_id_seq
    pg_restore: setting owner and privileges for SEQUENCE OWNED BY NonScienceObs_id_seq
    pg_restore: setting owner and privileges for TABLE Proposal
    pg_restore: setting owner and privileges for SEQUENCE Proposal_id_seq
    pg_restore: setting owner and privileges for SEQUENCE OWNED BY Proposal_id_seq
    pg_restore: setting owner and privileges for TABLE ScheduleItem
    pg_restore: setting owner and privileges for SEQUENCE ScheduleItem_id_seq
    pg_restore: setting owner and privileges for SEQUENCE OWNED BY ScheduleItem_id_seq
    pg_restore: setting owner and privileges for TABLE ScienceObs
    pg_restore: setting owner and privileges for SEQUENCE ScienceObs_id_seq
    pg_restore: setting owner and privileges for SEQUENCE OWNED BY ScienceObs_id_seq
    pg_restore: setting owner and privileges for DEFAULT id
    pg_restore: setting owner and privileges for DEFAULT id
    pg_restore: setting owner and privileges for DEFAULT id
    pg_restore: setting owner and privileges for DEFAULT id
    pg_restore: setting owner and privileges for TABLE DATA NonScienceObs
    pg_restore: setting owner and privileges for SEQUENCE SET NonScienceObs_id_seq
    pg_restore: setting owner and privileges for TABLE DATA Proposal
    pg_restore: setting owner and privileges for SEQUENCE SET Proposal_id_seq
    pg_restore: setting owner and privileges for TABLE DATA ScheduleItem
    pg_restore: setting owner and privileges for SEQUENCE SET ScheduleItem_id_seq
    pg_restore: setting owner and privileges for TABLE DATA ScienceObs
    pg_restore: setting owner and privileges for SEQUENCE SET ScienceObs_id_seq
    pg_restore: setting owner and privileges for CONSTRAINT NonScienceObsIdConstraint
    pg_restore: setting owner and privileges for CONSTRAINT NonScienceObs_pkey
    pg_restore: setting owner and privileges for CONSTRAINT PropConstraint
    pg_restore: setting owner and privileges for CONSTRAINT Proposal_pkey
    pg_restore: setting owner and privileges for CONSTRAINT ScheduleItem_pkey
    pg_restore: setting owner and privileges for CONSTRAINT ScheduleitemObsIdConstraint
    pg_restore: setting owner and privileges for CONSTRAINT ScienceObsIdConstraint
    pg_restore: setting owner and privileges for CONSTRAINT ScienceObs_pkey
    WARNING: errors ignored on restore: 3
    Password: 
     !    WARNING: Extensions in newly created target database differ from existing source database.
     !    
     !    Target extensions:
     !     extname 
     !    ---------
     !     plpgsql
     !    (1 row)
     !    
     !    
     !    Source extensions:
     !      extname  
     !    -----------
     !     adminpack
     !     plpgsql
     !    (2 rows)
     !    
     !    
     !    HINT: You should review output to ensure that any errors
     !    ignored are acceptable - entire tables may have been missed, where a dependency
     !    could not be resolved. You may need to to install a postgresql-contrib package
     !    and retry.

A quick check:

    % heroku pg:psql---> Connecting to HEROKU_POSTGRESQL_ROSE_URL (DATABASE_URL)
    psql (9.3.4, server 9.3.3)
    SSL connection (cipher: DHE-RSA-AES256-SHA, bits: 256)
    Type "help" for help.

    chandraobs-devel::ROSE=> select count(*) from "ScienceObs";
     count 
    -------
        62
    (1 row)

    chandraobs-devel::ROSE=> \q

To check what's going on (query-wise):

    % heroku pg:ps
     pid | state | source | running_for | waiting | query 
    -----+-------+--------+-------------+---------+-------
    (0 rows)

The database can be cleared out with

    % heroku pg:reset HEROKU_POSTGRESQL_ROSE --confirm chandraobs-devel

