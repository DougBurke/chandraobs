# Can we use rel8?

Check we can connect:

    % nix-shell
    % db=`heroku config:get --app chandraobservatory DATABASE_URL`
    % psql $db
    psql (11.12, server 9.6.22)
    SSL connection (protocol: TLSv1.2, cipher: ECDHE-RSA-AES256-GCM-SHA384, bits: 256, compression: off)
    Type "help" for help.

    d58ekh1qbn2go2=> select count(*) from "ProposalAbstract";
     count
    -------
      5107
    (1 row)

    d58ekh1qbn2go2=> \q
	
We can view the schema with

    d58ekh1qbn2go2=> SELECT schema_name FROM information_schema.schemata;
        schema_name
    --------------------
     pg_catalog
     information_schema
     public
    (3 rows)

    d58ekh1qbn2go2=> \dt
                         List of relations
     Schema |          Name           | Type  |     Owner
    --------+-------------------------+-------+----------------
     public | InvalidObsId            | table | nsiqoahowxuafe
     public | List##Telescope         | table | nsiqoahowxuafe
     public | List##Telescope#values  | table | nsiqoahowxuafe
     public | MetaData                | table | nsiqoahowxuafe
     public | MissingProposalAbstract | table | nsiqoahowxuafe
     public | NonScienceObs           | table | nsiqoahowxuafe
     public | Proposal                | table | nsiqoahowxuafe
     public | ProposalAbstract        | table | nsiqoahowxuafe
     public | ScienceObs              | table | nsiqoahowxuafe
     public | ShortTermSchedule       | table | nsiqoahowxuafe
     public | SimbadInfo              | table | nsiqoahowxuafe
     public | SimbadMatch             | table | nsiqoahowxuafe
     public | SimbadNoMatch           | table | nsiqoahowxuafe
    (13 rows)

    d58ekh1qbn2go2=> \d "ProposalAbstract"
                                        Table "public.ProposalAbstract"
       Column   |       Type        | Collation | Nullable |                    Default
    ------------+-------------------+-----------+----------+------------------------------------------------
     id         | bigint            |           | not null | nextval('"ProposalAbstract_id_seq"'::regclass)
     paNum      | bigint            |           | not null |
     paTitle    | character varying |           | not null |
     paAbstract | character varying |           | not null |
    Indexes:
        "ProposalAbstract_pkey" PRIMARY KEY, btree (id)
        "PropAbstractConstraint" UNIQUE CONSTRAINT, btree ("paNum")

