
Simbad query - to http://simbad.harvard.edu/simbad/sim-script but how to encode the script

can we get the object type and ... ?

::script::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

votable {main_id,coordinates}
votable open
query id m31
votable close

::console:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C.D.S.  -  SIMBAD4 rel 1.211  -  2014.06.08EDT17:06:32
total execution time: 0.207 secs
simbatch done

::data::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

<?xml version="1.0" encoding="UTF-8"?>
<VOTABLE xmlns="http://www.ivoa.net/xml/VOTable/v1.2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.2" version="1.2">
<DEFINITIONS>
<COOSYS ID="COOSYS" equinox="2000" epoch="J2000" system="ICRS"/>
</DEFINITIONS>
<RESOURCE name="Simbad query" type="results">
<TABLE ID="SimbadScript" name="default"><DESCRIPTION>Simbad script executed on 2014.06.08EDT17:06:32</DESCRIPTION>
<FIELD ID="MAIN_ID" name="MAIN_ID" datatype="char" width="22" ucd="meta.id;meta.main" arraysize="*">
<DESCRIPTION>Main identifier for an object</DESCRIPTION>
<LINK value="${MAIN_ID}" href="http://simbad.u-strasbg.fr/simbad/sim-id?Ident=${MAIN_ID}&amp;NbIdent=1"/>
</FIELD>
<FIELD ID="RA" name="RA" datatype="char" precision="8" width="13" ucd="pos.eq.ra;meta.main" arraysize="13" unit="&quot;h:m:s&quot;">
<DESCRIPTION>Right ascension</DESCRIPTION>
</FIELD>
<FIELD ID="DEC" name="DEC" datatype="char" precision="8" width="13" ucd="pos.eq.dec;meta.main" arraysize="13" unit="&quot;d:m:s&quot;">
<DESCRIPTION>Declination</DESCRIPTION>
</FIELD>
<FIELD ID="RA_PREC" name="RA_PREC" datatype="short" width="2">
<DESCRIPTION>Right ascension precision</DESCRIPTION>
</FIELD>
<FIELD ID="DEC_PREC" name="DEC_PREC" datatype="short" width="2">
<DESCRIPTION>Declination precision</DESCRIPTION>
</FIELD>
<FIELD ID="COO_ERR_MAJA" name="COO_ERR_MAJA" datatype="float" precision="3" width="6" ucd="phys.angSize.smajAxis;pos.errorEllipse;pos.eq" unit="mas">
<DESCRIPTION>Coordinate error major axis</DESCRIPTION>
</FIELD>
<FIELD ID="COO_ERR_MINA" name="COO_ERR_MINA" datatype="float" precision="3" width="6" ucd="phys.angSize.sminAxis;pos.errorEllipse;pos.eq" unit="mas">
<DESCRIPTION>Coordinate error minor axis</DESCRIPTION>
</FIELD>
<FIELD ID="COO_ERR_ANGLE" name="COO_ERR_ANGLE" datatype="short" width="3" ucd="pos.posAng;pos.errorEllipse;pos.eq" unit="deg">
<DESCRIPTION>Coordinate error angle</DESCRIPTION>
</FIELD>
<FIELD ID="COO_QUAL" name="COO_QUAL" datatype="char" width="1" ucd="meta.code.qual;pos.eq">
<DESCRIPTION>Coordinate quality</DESCRIPTION>
</FIELD>
<FIELD ID="COO_WAVELENGTH" name="COO_WAVELENGTH" datatype="char" width="1" ucd="instr.bandpass;pos.eq">
<DESCRIPTION>Wavelength class for the origin of the coordinates (R,I,V,U,X,G)</DESCRIPTION>
</FIELD>
<FIELD ID="COO_BIBCODE" name="COO_BIBCODE" datatype="char" width="19" ucd="meta.bib.bibcode;pos.eq" arraysize="*">
<DESCRIPTION>Coordinate reference</DESCRIPTION>
</FIELD>
<DATA>
<TABLEDATA>
<TR><TD>M  31</TD><TD>00 42 44.330</TD><TD>+41 16 07.50</TD><TD>(7)</TD><TD>(7)</TD><TD></TD><TD></TD><TD></TD><TD>B</TD><TD>IR</TD><TD>2006AJ....131.1163S</TD></TR>

</TABLEDATA>

</DATA>
</TABLE>
</RESOURCE>
</VOTABLE>

Note that tabs are used to separate the fields in the format statement below
% cat m31.script
format object "%MAIN_ID	%OTYPE(V)	%COO(d;A D)\n"
query id m31
% curl --data-urlencode script@m31.script http://simbad.harvard.edu/simbad/sim-script
::script::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

format object "%MAIN_ID	%OTYPE(V)	%COO(d;A D)\n"
query id m31

::console:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C.D.S.  -  SIMBAD4 rel 1.211  -  2014.06.09EDT16:00:38
total execution time: 0.164 secs
simbatch done

::data::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

M  31	Galaxy	010.684708 +41.268750

