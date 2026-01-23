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

Create the account and database, e.g.

    % sudo -u postgres psql postgres
    [sudo] password for djburke: 
    psql (9.4.5)
    Type "help" for help.

    postgres=# \password postgres
    Enter new password: 
    Enter it again: 
    postgres=# \q

    % sudo -u postgres createdb chandraobs

Used pgadmin3/psql to drop the existing tables in the database, e.g.

    % psql --username=postgres --password --host=127.0.0.1 --dbname=chandraobs
    chandraobs=# drop table "ScienceObs";
    DROP TABLE
    chandraobs=# drop table "NonScienceObs";
    DROP TABLE
    chandraobs=# drop table "ScheduleItem";
    DROP TABLE

and then fill in the data from my hand-created version:

    % cabal run initdb
    ...

    The obscat tool needs an active CIAO installation:
    
    % cabal run obscat
    ...
    % cabal run querysimbad
    ...
    % cabal run getcurrent
    ...    

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

and locally with

    % psql --username=postgres --password --host=127.0.0.1 --dbname=chandraobs < drop-all.sql

To remove the database entirely (locally)

  % psql --username=postgres --password --host=127.0.0.1
Password for user postgres: 
psql (9.5.5, server 9.4.7)
SSL connection (protocol: TLSv1.2, cipher: ECDHE-RSA-AES256-GCM-SHA384, bits: 256, compression: off)
Type "help" for help.

postgres=# drop database chandraobs
postgres-# \q

or use dropdb

% dropdb -e --username=postgres --password --host=127.0.0.1 chandraobs
Password: 
DROP DATABASE chandraobs;
% 

### On OS-X

WIth a homebrew postgres installation

    % launchctl load -w ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
    % createuser postgres
	% createdb -Opostgres -Eutf8 chandraobs
	
STEPS:

% heroku pg:reset DATABASE_URL --confirm chandraobs-devel ; PGUSER=postgres PGPASSWORD=postgres heroku pg:push chandraobs DATABASE_URL --app chandraobs-devel

 which has apparently now changed due to

▸    Error: Multiple apps in git remotes
 ▸    Usage: heroku pg:reset DATABASE_URL --confirm chandraobs-devel --remote heroku-cedar-14
 ▸       or: heroku pg:reset DATABASE_URL --confirm chandraobs-devel --app chandraobs-devel-cedar-14
 ▸    
 ▸    Your local git repository has more than 1 app referenced in git remotes.
 ▸    Because of this, we can't determine which app you want to run this command against.
 ▸    Specify the app you want with --app or --remote.
 ▸    
 ▸    Heroku remotes in repo:
 ▸    heroku          (chandraobs-devel)
 ▸    heroku-cedar-14 (chandraobs-devel-cedar-14)
 ▸    
 ▸    https://devcenter.heroku.com/articles/multiple-environments
heroku-cli: Pushing chandraobs ---> sailing-quietly-1739
 ▸    Remote database is not empty. Please create a new database or use
 ▸    heroku pg:reset

to the following, which no-longer seems to ask for passwords (pghost
environment variable added to fix a problem whereby the push would fail)

% heroku pg:reset DATABASE_URL --confirm chandraobs-devel --app chandraobs-devel; PGUSER=postgres PGPASSWORD=postgres PGHOST=127.0.0.1 heroku pg:push chandraobs DATABASE_URL --app chandraobs-devel

Notes

% psql --username=postgres --password --host=127.0.0.1 --dbname=chandraobs

chandraobs=# select count(*) from "SimbadMatch";
 count 
-------
  1290
(1 row)

chandraobs=# \d+ "SimbadMatch"


## Storing information (the groundhog way)

How are "complex" types stored? In this case the "multi tel" field in ScienceObs

% heroku pg:psql -a chandraobservatory
..


Or

% psql $DATABASE_URL


chandraobservatory::DATABASE=> \dt
                     List of relations
 Schema |          Name           | Type  |     Owner      
--------+-------------------------+-------+----------------
 public | InvalidObsId            | table | u6910amfie4lt0
 public | List##Telescope         | table | u6910amfie4lt0
 public | List##Telescope#values  | table | u6910amfie4lt0
 public | MetaData                | table | u6910amfie4lt0
 public | MissingProposalAbstract | table | u6910amfie4lt0
 public | NonScienceObs           | table | u6910amfie4lt0
 public | Proposal                | table | u6910amfie4lt0
 public | ProposalAbstract        | table | u6910amfie4lt0
 public | ScienceObs              | table | u6910amfie4lt0
 public | ShortTermSchedule       | table | u6910amfie4lt0
 public | SimbadInfo              | table | u6910amfie4lt0
 public | SimbadMatch             | table | u6910amfie4lt0
 public | SimbadNoMatch           | table | u6910amfie4lt0

dak24kljssi6us=> select count (*) from "List##Telescope";
 count 
-------
 30095
(1 row)

dak24kljssi6us=> select count (*) from "List##Telescope#values";
 count 
-------
  2005
(1 row)

dak24kljssi6us=> select * from "List##Telescope" limit 1;
 id 
----
  1
(1 row)

dak24kljssi6us=> select * from "List##Telescope#values" limit 1;
 id | ord | value  
----+-----+--------
 22 |   0 | NuSTAR
(1 row)


So ##Telescope looks to be an index into the #values array. Although it is really not
clear. THe #values field has "id" and "ord", where ord=0, 1, 2 to simulate multi-tel
fields, but we also have some value fields with multiple telescopes:

dak24kljssi6us=> select distinct value from "List##Telescope#values";
                            value                            
-------------------------------------------------------------
 INTEGRAL, HESS, ATCA
 XTE, XMM
 HESS, ATCA
 NuStar
 NRAO-GBT
 vla, kpno
 CHANDRA, HST, AND FUSE
 ALMA, TESS, Hubble, Swift, duPont (Las Campanas), Evryscope
 VLA, NOAO/LCO
 JWST, Ground
 Australia Telescope Compact Array
 Kepler
 VLA,VLBA
 HST/STIS
 INAF, OAR Campo Imperatore, I
 HST
 XMM-Newton, Suzaku
 Ground
 HST,CXO
 kpno,vla
 XMM-Newton and INTEGRAL
 XMM - an application for Suzaku time has also been made
 FUSE
 HST, VLA+VLBA, MERLIN
 RXTE, Fermi GBM
 FLWO-ground
 Ground (VLBA)
 NRAO (required)
 HST/WFPC2 (if possible, not critical)
 VLA (NRAO)
 XMM-Newton, NuSTAR
 CARMA
 NUSTAR
 VLA, NuSTAR
 EVLA
 HST, JVLA
 VLTI-Gravity
 NRAO VLA
 CXO, VLA, HST
 NRAO/VLA
 Contemporaneous with proposed HST observation
 HST, SST, XMM, ASTRO-E2, GALEX, FUSE,CHIPS
 XMM-Newton, Suzaku, NuStar
 RXTE
 XMM
 JWST, Event Horizon Telescope, NuSTAR
 EventHorizonTelescope
 HST, FUSE, GROUND-BASED OPTICAL
 XRISM
 HST, CHANDRA, NOAO
 VLA, OPTICAL
 radio (will be coordinated on a best-effort basis by PI)
 NICER
 VLA, NICER, NUSTAR, Gemini
 OVRO/CARMA
 Hitomi
 ground - FLWO
 NuSTAR and Effelsberg
 VLA, VLBA
 SKINAKAS OBSERVATORY, CRETE, GREECE
 GROUND, ROSAT, EUVE
 If possible, radio telescope data will be taken.
 New Horizons, XMM-Newton, possibly HST
 Swift, LCO-FTP
 XMM-Newton, NuStar
 RXTE, XMM
 HST/JWST
 EVN
 HST and Ground (VLA, mm, mid-IR, optical, and gamma-ray)
 HST + WHT, MMT, HET AND/OR GEMINI
 XMM, RXTE, BEPPOSAX
 Corot
 HST, GROUND
 ATCA
 XMM-Newton,NuStar, Astro-H
 EVN/e-MERLIN
 Global VLBI (EVN, VLA, LBA combined)
 Suzaku
 CHANDRA, HST/STIS, GROUND FACILITIES
 STEWARD OBSERVATORY 2.3 M TELESCOPE
 NRAO VLA (likely Dec 2020-Jan 2021 window)
 VLA simultaneous within one day
 HST, NuSTAR
 NICER, NuSTAR
 Suzaku, HST
 NOAO/Gemini-North, NRAO/EVLA
 HST, FUSE, RXTE, GROUND-BASED
 Hubble Space Telescope
 GBT
 Swift
 HST, NOAO, VLA, MERLIN
 HST and VLA
 VERITAS, Swift, NuSTAR (potentially)
 INTEGRAL
 KPNO/4-m
 Ground-FLWO
 GROUND
 AUSTRAILIA TELESCOPE COMPACT ARRAY; OBSERVING EVERY 5 DAYS
 XMM-Newton, NuSTAR, Chandra
 MEarth
 SHANE 120 INCH (LICK OBSERVATORY, UCO)
 INTEGRAL GAMMA RAY SPACE OBSERVATORY
 Event Horizon Telescope, NuSTAR (if available)
 eROSITA  Performance Verification and Calibration Phase
 Chandra, HST
 Juno spacecraft
 VLBA
 Hubble
 GROUND (APO)
 IXPE
 XTE, GROUND
 EHT
 KPNO 4-m
 Effelsberg Radio Telescope
 GROUND (HET WHEN POSSIBLE)
 NuSTAR
 NusTAR
 JVLA
 XMM-NEWTON
 EVLA, XMM
 RXTE, Suzaku, Ground
 NuSTAR, Gemini, JVLA
 ATCA, NRAO
 JWST
 Ground (ATCA)
 Gran Telescopio Canarias
 Hubble, VLT/NACO and Gemini/NIRI
 VLT
 Event Horizon Telescope, NuSTAR
 EventHorizonTelescope, NuSTAR
 Event Horizon Telescope
 HST, CXO
 XXM-Newton
 XTE
 EVLA, SMARTS 1.3m
 ESO/Nustar (to be confirmed though)
 VERITAS, ground-based optical/IR/radio observatories
 INTEGRAL, RXTE, VLA, RATAN-600, Ryle Telescope, VLBA
 ALMA, ATCA
 RXTE, VLA, VLBA
 GROUND (HOBBY-EBERLY TELESCOPE)
 NRAO (and see remarks regarding XMM)
 HET and/or ARC
 GROUND (VLA)
 NRAO
 XMM-Newton, SWIFT
 GROUND, RXTE
 HST COS
 VLA
 NuSTAR, Keck, VLT, SMA, CSO, CARMA, ALMA, EHT, HESS, ATCA
 European VLBI Network
 NOAO: CTIO SMARTS telescope
 ALMA, Swift, NICER, Evryscope
 Venus Express
 VLA, Ground
 spitzer
 XMM, INTEGRAL
 NuSTAR, ATCA, VLT
 Hobby-Eberly Telescope
 Spitzer
 NuSTAR, Gemini, JVLA (or ATCA), NICER
 HST, FUSE
 CHIPS, SST
 IXPE, SMA
 Gemini/North
 XMM-Newton
 JWST, ALMA, NuSTAR
 NuSTAR, NICER
 ATCA, VLA, Ground-based (DDT submitted or in the making)


It's also not clear how "no value" is encoded.

In groundhog I used

   soMultiTelObs :: [Telescope]

where

newtype Telescope = Telescope { fromTelescope :: T.Text }
                  deriving Eq

Look at obsid 20021, 25041

So, for obsid 25041 we have the soMultiTel value is an index to List##Telescope,
which is the id for List##Telescope#value (but as List##Telescope only contains an
id field there is no obvious need for it).

dak24kljssi6us=> select "soObsId", "soMultiTel", "soMultiTelInt", "soMultiTelObs" from "ScienceObs" where "soMultiTelObs" = 726;
 soObsId | soMultiTel | soMultiTelInt | soMultiTelObs 
---------+------------+---------------+---------------
   20001 | f          |           0.1 |           726
(1 row)


So ObsId 20001 has soMultiTel == False but multiTelInt > 0!

dak24kljssi6us=> select * from "List##Telescope" where id = 726;
 id  
-----
 726
(1 row)

dak24kljssi6us=> select * from "List##Telescope#values" where id = 726;
 id  | ord | value 
-----+-----+-------
 726 |   0 | XMM
(1 row)


So is the database confused?

dak24kljssi6us=> select DISTINCT id from "List##Telescope#values" ORDER BY id;

write out to distinct-id.txt

So, we have soMultiTel = TRUE and soMultiTelInt = 0

dak24kljssi6us=> select count(*) from "ScienceObs" WHERE "soMultiTel" = true and "soMultiTelInt" < 0.01;
 count 
-------
   532
(1 row)

dak24kljssi6us=> select count(*) from "ScienceObs" WHERE "soMultiTel" = true and "soMultiTelInt" <= 0;
 count 
-------
   532
(1 row)

dak24kljssi6us=> select DISTINCT "soMultiTelInt" from "ScienceObs" WHERE "soMultiTel" = true ORDER BY "soMultiTelInt";
 soMultiTelInt 
---------------
             0
          0.04
          0.05
           0.1
          0.12
         0.167
          0.17
           0.2
          0.25
           0.3
          0.35
       0.41667
         0.463
           0.5
           0.6
        0.6671
           0.9
             1
...
            80
            90
           100
           120
           160
           168
           180
           363
           364
(54 rows)


dak24kljssi6us=> select DISTINCT "soMultiTelInt" from "ScienceObs" WHERE "soMultiTel" = false ORDER BY "soMultiTelInt";
 soMultiTelInt 
---------------
             0
          0.02
           0.1
          0.25
          0.29
           0.5
             1
           1.4
...
           190
           220
           364
(29 rows)


So, is the multiTel stuff just completely borked?

Let's look at the "longest" multitelint fields, which have both True and False.

dak24kljssi6us=> select "soObsId", "soStatus", "soMultiTel", "soMultiTelInt", "soMultiTelObs" from "ScienceObs" WHERE "soMultiTelInt" > 300;
 soObsId | soStatus | soMultiTel | soMultiTelInt | soMultiTelObs 
---------+----------+------------+---------------+---------------
   10718 | archived | t          |           363 |          7193
   10717 | archived | t          |           363 |          7257
   10716 | archived | t          |           363 |          7366
   10715 | archived | t          |           363 |          7376
   18301 | archived | f          |           364 |         32439
   18302 | archived | f          |           364 |         32440
   10719 | archived | t          |           363 |         17909
   18676 | archived | f          |           364 |         32546
   18677 | archived | f          |           364 |         32547
   18678 | archived | f          |           364 |         32548
   25253 | observed | t          |           364 |         42710
   27802 | observed | t          |           364 |         42713
   27803 | observed | t          |           364 |         42719
(13 rows)


AHA: soMultiTelInt should really be "maximum interval" and is related to "something" but it need
not be multi-telescope.

The table may also include "deleted" entries (all the Listg##Telescope#values ord > 0
entries, from what I can tell).

dak24kljssi6us=> \d "MetaData"
                                           Table "public.MetaData"
     Column     |            Type             | Collation | Nullable |                Default                 
----------------+-----------------------------+-----------+----------+----------------------------------------
 id             | bigint                      |           | not null | nextval('"MetaData_id_seq"'::regclass)
 mdLastModified | timestamp without time zone |           | not null | 
Indexes:
    "MetaData_pkey" PRIMARY KEY, btree (id)
    "MetaDataLastModifiedConstraint" UNIQUE CONSTRAINT, btree ("mdLastModified")






                                            Table "public.ScienceObs"
     Column      |            Type             | Collation | Nullable |                 Default                  
-----------------+-----------------------------+-----------+----------+------------------------------------------
 id              | bigint                      |           | not null | nextval('"ScienceObs_id_seq"'::regclass)
 soSequence      | bigint                      |           | not null | 
 soProposal      | bigint                      |           | not null | 
 soStatus        | character varying           |           | not null | 
 soObsId         | bigint                      |           | not null | 
 soTarget        | character varying           |           | not null | 
 soStartTime     | timestamp without time zone |           |          | 
 soApprovedTime  | double precision            |           | not null | 
 soObservedTime  | double precision            |           |          | 
 soPublicRelease | timestamp without time zone |           |          | 
 soTimeCritical  | character varying           |           | not null | 
 soMonitor       | character varying           |           | not null | 
 soConstrained   | character varying           |           | not null | 
 soInstrument    | character varying           |           | not null | 
 soGrating       | character varying           |           | not null | 
 soDetector      | character varying           |           |          | 
 soDataMode      | character varying           |           |          | 
 soACISI0        | character varying           |           | not null | 
 soACISI1        | character varying           |           | not null | 
 soACISI2        | character varying           |           | not null | 
 soACISI3        | character varying           |           | not null | 
 soACISS0        | character varying           |           | not null | 
 soACISS1        | character varying           |           | not null | 
 soACISS2        | character varying           |           | not null | 
 soACISS3        | character varying           |           | not null | 
 soACISS4        | character varying           |           | not null | 
 soACISS5        | character varying           |           | not null | 
 soJointWith     | character varying           |           |          | 
 soJointHST      | double precision            |           |          | 
 soJointNOAO     | double precision            |           |          | 
 soJointNRAO     | double precision            |           |          | 
 soJointRXTE     | double precision            |           |          | 
 soJointSPITZER  | double precision            |           |          | 
 soJointSUZAKU   | double precision            |           |          | 
 soJointXMM      | double precision            |           |          | 
 soJointSWIFT    | double precision            |           |          | 
 soJointNUSTAR   | double precision            |           |          | 
 soMultiTel      | boolean                     |           | not null | 
 soMultiTelInt   | double precision            |           | not null | 
 soMultiTelObs   | bigint                      |           | not null | 
 soTOO           | character varying           |           |          | 
 soRA            | double precision            |           | not null | 
 soDec           | double precision            |           | not null | 
 soConstellation | character varying           |           | not null | 
 soRoll          | double precision            |           | not null | 
 soSubArrayStart | bigint                      |           |          | 
 soSubArraySize  | bigint                      |           |          | 
Indexes:
    "ScienceObs_pkey" PRIMARY KEY, btree (id)
    "ScienceObsIdConstraint" UNIQUE CONSTRAINT, btree ("soObsId")
Foreign-key constraints:
    "ScienceObs_soMultiTelObs_fkey" FOREIGN KEY ("soMultiTelObs") REFERENCES "List##Telescope"(id)
Triggers:
    "ScienceObs" AFTER DELETE ON "ScienceObs" FOR EACH ROW EXECUTE FUNCTION "ScienceObs"()
    "ScienceObs#soMultiTelObs" AFTER UPDATE OF "soMultiTelObs" ON "ScienceObs" FOR EACH ROW EXECUTE FUNCTION "ScienceObs#soMultiTelObs"()

dak24kljssi6us=> \d "List##Telescope"
                             Table "public.List##Telescope"
 Column |  Type  | Collation | Nullable |                    Default                    
--------+--------+-----------+----------+-----------------------------------------------
 id     | bigint |           | not null | nextval('"List##Telescope_id_seq"'::regclass)
Indexes:
    "List##Telescope_pkey" PRIMARY KEY, btree (id)
Referenced by:
    TABLE ""List##Telescope#values"" CONSTRAINT "List##Telescope#values_id_fkey" FOREIGN KEY (id) REFERENCES "List##Telescope"(id) ON DELETE CASCADE
    TABLE ""ScienceObs"" CONSTRAINT "ScienceObs_soMultiTelObs_fkey" FOREIGN KEY ("soMultiTelObs") REFERENCES "List##Telescope"(id)

dak24kljssi6us=> \d "List##Telescope#values"
            Table "public.List##Telescope#values"
 Column |       Type        | Collation | Nullable | Default 
--------+-------------------+-----------+----------+---------
 id     | bigint            |           | not null | 
 ord    | integer           |           | not null | 
 value  | character varying |           | not null | 
Foreign-key constraints:
    "List##Telescope#values_id_fkey" FOREIGN KEY (id) REFERENCES "List##Telescope"(id) ON DELETE CASCADE



dak24kljssi6us=> \d "SimbadInfo";
                                   Table "public.SimbadInfo"
  Column  |       Type        | Collation | Nullable |                 Default                  
----------+-------------------+-----------+----------+------------------------------------------
 id       | bigint            |           | not null | nextval('"SimbadInfo_id_seq"'::regclass)
 smiName  | character varying |           | not null | 
 smiType3 | character varying |           | not null | 
 smiType  | character varying |           | not null | 
Indexes:
    "SimbadInfo_pkey" PRIMARY KEY, btree (id)
    "SimbadInfoConstraint" UNIQUE CONSTRAINT, btree ("smiName")
Referenced by:
    TABLE ""SimbadMatch"" CONSTRAINT "SimbadMatch_smmInfo_fkey" FOREIGN KEY ("smmInfo") REFERENCES "SimbadInfo"(id)

dak24kljssi6us=> \d "SimbadMatch";
                                           Table "public.SimbadMatch"
     Column     |            Type             | Collation | Nullable |                  Default                  
----------------+-----------------------------+-----------+----------+-------------------------------------------
 id             | bigint                      |           | not null | nextval('"SimbadMatch_id_seq"'::regclass)
 smmTarget      | character varying           |           | not null | 
 smmSearchTerm  | character varying           |           | not null | 
 smmInfo        | bigint                      |           | not null | 
 smmLastChecked | timestamp without time zone |           | not null | 
Indexes:
    "SimbadMatch_pkey" PRIMARY KEY, btree (id)
    "SimbadMatchConstraint" UNIQUE CONSTRAINT, btree ("smmTarget")
Foreign-key constraints:
    "SimbadMatch_smmInfo_fkey" FOREIGN KEY ("smmInfo") REFERENCES "SimbadInfo"(id)

dak24kljssi6us=> \d "SimbadNoMatch";
                                           Table "public.SimbadNoMatch"
     Column     |            Type             | Collation | Nullable |                   Default                   
----------------+-----------------------------+-----------+----------+---------------------------------------------
 id             | bigint                      |           | not null | nextval('"SimbadNoMatch_id_seq"'::regclass)
 smnTarget      | character varying           |           | not null | 
 smnSearchTerm  | character varying           |           | not null | 
 smnLastChecked | timestamp without time zone |           | not null | 
Indexes:
    "SimbadNoMatch_pkey" PRIMARY KEY, btree (id)
    "SimbadNoMatchConstraint" UNIQUE CONSTRAINT, btree ("smnTarget")
