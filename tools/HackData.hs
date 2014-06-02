-- | An example data set taken from the Chandra Short-Term schedule and
--   manually converted.
--
module HackData (testSchedule) where

import Control.Applicative ((<$>))

import Types

{-
-- | This requires CIAO, since it uses prop_precess. It would
--   be quicker to run on an array of sources, but for now
--   run it per source.
--
getConstellation ::
  Double     -- ra J2000 decimal degrees
  -> Double  -- dec J2000 decimal degrees
  -> IO (Maybe String) -- name of the constellation
getConstellation ra dec = 
  let x = y
      f = "prop_precess from j/deg to con:"
  in undefined
-}

-- wrapper to create a record
toR :: 
  Maybe Int
  -> ObsName
  -> Maybe Int  -- number of constraints; probably dropping
  -> String     -- target name
  -> String     -- start time (passed to toCTime)
  -> Double     -- exposure time
  -> Maybe Instrument
  -> Maybe Grating
  -> Double     -- ra
  -> Double     -- dec
  -> Double     -- roll
  -> Double     -- pitch
  -> Double     -- slew
  -> Record
toR mseq obs mcon tgt stTime eval minst mgrat ra dec = 
  Record (Sequence <$> mseq) obs mcon tgt (toCTime stTime) eval minst mgrat (RA ra) (Dec dec)

testSchedule :: [Record]
testSchedule = [
 toR (Just 501802) (ObsId 14481) (Just 1) "Cassiopeia A" "2014:132:03:08:49.668" 50.0 (Just ACISS) (Just NONE) 350.8962  58.8246  74.97  58.22  73.46

 , toR (Just 801422) (ObsId 16280) (Just 0) "G249.87-39.86" "2014:132:17:54:22.746" 7.0 (Just ACISI) (Just NONE) 72.4564 (-44.6653) 204.13  66.28 165.23

 , toR (Just 901116) (ObsId 16194) (Just 0) "30 Doradus" "2014:132:20:19:02.746"  31.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.84  25.34

 , toR Nothing (SpecialObs "T_E39") Nothing "CAL-ER (52778)" "2014:133:05:50:04.996"   0.1 Nothing Nothing 219.8433 (-42.5680) 337.85 154.31  63.59

 , toR Nothing (SpecialObs "GG_39") Nothing "CAL-ER (52777)" "2014:133:08:03:00.000"   0.1 Nothing Nothing 236.0147   6.4846 166.37 154.45 177.31

 , toR Nothing (SpecialObs "G1_39") Nothing "CAL-ER (52776)" "2014:133:12:52:26.688"   0.3 Nothing Nothing 19.0000  60.0000  41.91  47.19 107.40

 , toR Nothing (SpecialObs "T_X40") Nothing "CAL-ER (52775)" "2014:133:15:16:38.193"   0.1 Nothing Nothing 224.0000   6.0000 193.94 154.84 111.53

 , toR (Just 502237) (ObsId 16004) (Just 0) "Kepler's Supernova R" "2014:133:18:20:16.193" 104.0 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 149.48 113.86

 , toR (Just 702754) (ObsId 14517) (Just 1) "Q2237+0305" "2014:134:23:46:47.815"  30.0 (Just ACISS) (Just NONE) 340.1442   3.3410 108.61  71.29  79.62

 , toR (Just 901116) (ObsId 16615) (Just 0) "30 Doradus" "2014:135:08:44:31.003"  45.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.97  99.99

 , toR Nothing (SpecialObs "T_E40") Nothing "CAL-ER (52773)" "2014:135:22:09:14.231"   0.1 Nothing Nothing 220.1000 (-42.1000) 331.99 154.60  64.54

 , toR Nothing (SpecialObs "GG_40") Nothing "CAL-ER (52772)" "2014:136:00:22:00.000"   0.3 Nothing Nothing 67.0000 (-26.0000) 198.83  47.13 107.51

 , toR Nothing (SpecialObs "G1_40") Nothing "CAL-ER (52771)" "2014:136:01:43:19.088"   0.8 Nothing Nothing 105.0000  11.0000 254.38  51.09  71.19

 , toR Nothing (SpecialObs "G2_40") Nothing "CAL-ER (52770)" "2014:136:04:29:42.928"   0.5 Nothing Nothing  62.0271 (-29.2154) 191.69  49.08  78.13

 , toR Nothing (SpecialObs "T_X41") Nothing "CAL-ER (52769)" "2014:136:06:59:34.670"   0.1 Nothing Nothing 0.7666   8.5993 107.55  51.42  96.58

 , toR (Just 502237) (ObsId 16614) (Just 0) "Kepler's Supernova R" "2014:136:09:03:12.659"  36.8 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 152.00 100.61

 , toR (Just 601116) (ObsId 16028) (Just 1) "NGC 300" "2014:136:19:53:38.301"  65.0 (Just ACISI) (Just NONE)  13.7470 (-37.6893) 130.01  67.94  97.17

 , toR (Just 801381) (ObsId 16525) (Just 0) "Abell S0295" "2014:137:14:24:15.301"  45.0 (Just ACISI) (Just NONE)  41.3955 (-53.0284) 167.50  73.16  29.99

  -- TODO: I think the SpecialObs value here is an error in the input 
 , toR Nothing (SpecialObs "I5190") Nothing "CAL-ER (52768)" "2014:138:03:36:44.209"   0.6 Nothing Nothing 190.2715  (-5.1604) 247.67 134.54 121.42


 , toR (Just 801315) (ObsId 15190) (Just 0) "A1612" "2014:138:04:14:01.209"  29.1 (Just ACISI) (Just NONE) 191.9626  (-2.7668) 229.62 135.10  18.40

 , toR Nothing (SpecialObs "T_E41") Nothing "CAL-ER (52766)" "2014:138:13:12:14.645"   0.1 Nothing Nothing 219.7761 (-42.5475) 334.00 153.64 101.50

 , toR Nothing (SpecialObs "GG_41") Nothing "CAL-ER (52765)" "2014:138:16:08:30.968"  10.0 Nothing Nothing  237.0000  10.0000 176.47 150.36 164.87   
 , toR Nothing (SpecialObs "G1_41") Nothing "CAL-ER (52764)" "2014:138:19:03:40.968"  10.0 Nothing Nothing  237.0000  10.0000 176.74 150.33   0.26   
 , toR Nothing (SpecialObs "T_X42") Nothing "CAL-ER (52763)" "2014:138:22:58:55.645"   0.1 Nothing Nothing  258.0000 (-45.0000)  43.72 148.52 145.40   
 , toR (Just 601090) (ObsId 15384) (Just 0) "ngc6744" "2014:139:01:19:16.645"  53.5 (Just ACISI) (Just NONE) 287.4564 (-63.8721)  67.01 123.91  25.07
 , toR (Just 502105) (ObsId 15872) (Just 0) "Short GRB ToO" "2014:139:16:57:15.388"  20.0 (Just ACISS) (Just NONE) 253.0210  39.9670 161.96 118.24 138.66
 , toR (Just 502218) (ObsId 15985) (Just 0) "PSR J2307+2225" "2014:139:23:03:34.425"   3.5 (Just ACISS) (Just NONE) 346.9388  22.4116 102.02  64.30  78.68
 , toR (Just 601126) (ObsId 16214) (Just 3) "Sgr A*" "2014:140:00:38:17.736"  50.0 (Just ACISS) (Just NONE) 266.4253 (-29.0313)  80.00 151.35  94.00
 , toR (Just 801381) (ObsId 16524) (Just 0) "Abell S0295" "2014:140:15:08:47.380"  45.1 (Just ACISI) (Just NONE)  41.3979 (-53.0335) 152.03  74.26  97.47
 , toR Nothing (SpecialObs "T_E42") Nothing "CAL-ER (52761)" "2014:141:04:25:19.493"   0.1 Nothing Nothing  244.0000 (-50.0000)  11.91 149.68  75.93   
 , toR Nothing (SpecialObs "GG_42") Nothing "CAL-ER (52760)" "2014:141:07:14:35.865"  10.0 Nothing Nothing  270.0000 (-27.0000)  83.86 149.77  62.49   
 , toR Nothing (SpecialObs "G1_42") Nothing "CAL-ER (52759)" "2014:141:10:36:51.599"   0.1 Nothing Nothing  224.0000 (-47.0000) 332.39 150.88  90.38   
 , toR Nothing (SpecialObs "G2_42") Nothing "CAL-ER (52758)" "2014:141:11:16:55.400"   3.0 Nothing Nothing   95.5000 (-15.0000) 227.43  50.98 103.04   
 , toR Nothing (SpecialObs "G3_42") Nothing "CAL-ER (52757)" "2014:141:12:45:19.039"   3.0 Nothing Nothing  224.0000 (-47.0000) 332.19 150.85 103.02   
 , toR Nothing (SpecialObs "T_X43") Nothing "CAL-ER (52756)" "2014:141:13:53:30.726"   0.1 Nothing Nothing  233.0000 (-54.0000) 351.45 145.97  15.24   
 , toR (Just 901099) (ObsId 16159) (Just 0) "CL J1449+0856" "2014:141:16:38:46.130"  96.0 (Just ACISS) (Just NONE) 222.3214   8.9591 208.04 146.91 145.04
 , toR (Just 401442) (ObsId 14609) (Just 2) "M22" "2014:142:20:03:50.934"  85.9 (Just ACISS) (Just NONE) 279.1145 (-23.9246)  92.54 143.10 133.11
 , toR Nothing (SpecialObs "T_E43") Nothing "CAL-ER (52754)" "2014:143:20:43:44.271"   0.1 Nothing Nothing   22.0000 (-19.0000) 134.59  54.60  93.70   
 , toR Nothing (SpecialObs "T_X44") Nothing "CAL-ER (52753)" "2014:143:22:00:00.000"   0.1 Nothing Nothing  277.0001 (-85.0000)  37.86 114.63  72.50   
 , toR (Just 901116) (ObsId 16195) (Just 0) "30 Doradus" "2014:144:14:28:00.000"  45.0 (Just ACISI) (Just NONE)  84.7062 (-69.0842) 201.98  91.43  26.04
 , toR (Just 401568) (ObsId 15771) (Just 2) "Holmberg II X-1" "2014:145:03:47:33.062"  13.0 (Just ACISS) (Just NONE) 124.8168  70.7224 288.60  61.63 153.23
 , toR (Just 502271) (ObsId 16618) (Just 0) "GRB140515A" "2014:145:07:54:52.554"  20.0 (Just ACISS) (Just NONE) 186.0587  15.1260 256.00 114.47  68.21
 , toR (Just 501948) (ObsId 16489) (Just 0) "HESS J1809-193" "2014:145:14:19:24.593"  65.7 (Just ACISI) (Just NONE) 272.4377 (-19.2951) 108.00 151.51 160.65
 , toR (Just 601138) (ObsId 16294) (Just 1) "M31 BHXNe" "2014:146:09:13:17.508"   5.0 (Just ACISI) (Just NONE)  10.6971  41.2515  81.74  48.15 108.67
 , toR Nothing (SpecialObs "T_E44") Nothing "CAL-ER (52750)" "2014:146:11:30:04.215"   0.1 Nothing Nothing  262.8282  (-0.9587) 137.49 152.06 104.00   
 , toR Nothing (SpecialObs "GG_44") Nothing "CAL-ER (52749)" "2014:146:13:10:09.653"   0.1 Nothing Nothing  262.8282  (-0.9587) 137.61 152.10   0.12   
 , toR Nothing (SpecialObs "G1_44") Nothing "CAL-ER (52748)" "2014:146:17:26:04.176"   0.5 Nothing Nothing  219.0000  (-6.0000) 234.80 152.04 106.83   
 , toR Nothing (SpecialObs "T_X45") Nothing "CAL-ER (52747)" "2014:146:21:24:46.655"   0.1 Nothing Nothing  260.0000 (-48.0000)  32.17 150.06 141.77   
 , toR (Just 801409) (ObsId 16223) (Just 0) "Centaurus cluster" "2014:146:23:49:18.215" 181.3 (Just ACISS) (Just NONE) 192.1895 (-41.3162) 269.99 132.53  83.90
 , toR Nothing (SpecialObs "T_E45") Nothing "CAL-ER (52745)" "2014:149:02:42:39.112"   0.1 Nothing Nothing  219.7414 (-29.5202) 282.86 155.35  25.43   
 , toR Nothing (SpecialObs "GG_45") Nothing "CAL-ER (52744)" "2014:149:05:29:25.804"   1.3 Nothing Nothing  266.0000   0.0000 138.84 150.77 159.33   
 , toR Nothing (SpecialObs "G1_45") Nothing "CAL-ER (52743)" "2014:149:09:49:55.071"   0.7 Nothing Nothing  220.0640 (-16.3622) 253.85 154.96 127.44   
 , toR Nothing (SpecialObs "T_X46") Nothing "CAL-ER (52742)" "2014:149:12:13:32.068"   0.1 Nothing Nothing  232.0000  (-1.0000) 212.72 155.23  46.92   
 , toR (Just 502223) (ObsId 15990) (Just 1) "Swift J1822.3-1606" "2014:149:14:07:22.697"  14.0 (Just ACISS) (Just NONE) 275.5899 (-16.0941)  96.50 151.67 127.87
 , toR (Just 702724) (ObsId 14487) (Just 1) "QJ0158-4325" "2014:149:18:38:07.037"  19.0 (Just ACISS) (Just NONE)  29.7062 (-43.4211) 144.59  73.25  98.58
 , toR (Just 901116) (ObsId 16196) (Just 0) "30 Doradus" "2014:150:00:22:47.037"  68.0 (Just ACISI) (Just NONE)  84.7062 (-69.0842) 202.00  91.68  38.88
 , toR (Just 901105) (ObsId 16165) (Just 0) "1RXS J174559.6-37005" "2014:150:19:48:26.306"   4.0 (Just ACISS) (Just NONE) 266.4876 (-37.0384)  41.99 157.53  75.70
 , toR (Just 901117) (ObsId 16286) (Just 0) "ESO512-009" "2014:150:21:33:29.558"  13.0 (Just ACISS) (Just NONE) 219.8258 (-25.4505) 279.99 154.43 102.99
 , toR (Just 901116) (ObsId 16617) (Just 0) "30 Doradus" "2014:151:01:45:49.205"  59.6 (Just ACISI) (Just NONE)  84.7062 (-69.0842) 202.00  91.73  90.72
 , toR Nothing (SpecialObs "T_E46") Nothing "CAL-ER (52740)" "2014:151:19:00:26.007"   0.1 Nothing Nothing  244.2000 (-51.0000) 352.05 150.76  59.31   
 , toR Nothing (SpecialObs "GG_46") Nothing "CAL-ER (52739)" "2014:151:20:59:05.211"   1.6 Nothing Nothing  264.5513  (-3.5085) 141.15 155.84 142.92   
 , toR Nothing (SpecialObs "T_X47") Nothing "CAL-ER (52738)" "2014:152:02:09:26.364"   0.1 Nothing Nothing  222.6617 (-15.5862) 251.42 154.54 122.33   
 , toR (Just 502102) (ObsId 15869) (Just 0) "SN 2010jl" "2014:152:06:24:29.221"  40.0 (Just ACISS) (Just NONE) 145.7188   9.5194 249.67  74.29  80.25

 , toR (Just 801377)    (ObsId 16123) (Just 0) "RXJ1416.4+2315 Offse" "2014:152:18:01:31.984"   5.0 (Just ACISI) (Just NONE) 214.1596  23.4419 219.63 123.03  66.96
 , toR (Just 702978)    (ObsId 16088) (Just 0) "SDSSJ162210.11+07021" "2014:152:19:50:08.984"  65.8 (Just ACISS) (Just NONE) 245.5584   7.0526 194.00 150.64  38.21
 , toR (Just 702737)    (ObsId 14500) (Just 1)      "SDSS1004+4112" "2014:153:14:43:02.164"  25.0 (Just ACISS) (Just NONE) 151.1333  41.2359 258.45  68.95  89.64
 , toR (Just 702990)    (ObsId 16102) (Just 0)   "J011341.1+010608" "2014:153:22:30:21.065"  15.0 (Just ACISS) (Just NONE)  18.4413   1.0879 116.60  54.90 158.17
 , toR (Just 601122)    (ObsId 16210) (Just 3)             "Sgr A*" "2014:154:03:20:13.610"  18.7 (Just ACISS) (Just NONE) 266.4234 (-29.0318)  76.00 164.39 109.69
 , toR Nothing     (SpecialObs "I6561") Nothing       "CAL-ER (52736)" "2014:154:08:51:04.836"   0.3 Nothing Nothing  289.0000 (-26.0000)  92.11 145.28  20.96   
 , toR Nothing     (SpecialObs "T_E47") Nothing       "CAL-ER (52734)" "2014:154:10:03:16.248"   0.1 Nothing Nothing  289.0000 (-26.0000)  92.10 145.33   0.01   
 , toR Nothing     (SpecialObs "GG_47") Nothing       "CAL-ER (52733)" "2014:154:11:35:08.830"   0.8 Nothing Nothing  280.0000 (-15.0000) 109.36 151.77  24.64   
 , toR Nothing     (SpecialObs "G1_47") Nothing       "CAL-ER (52732)" "2014:154:14:17:00.106"   8.0 Nothing Nothing  337.0000 (-27.5000) 108.30 103.68  58.62   
 , toR Nothing     (SpecialObs "G2_47") Nothing       "CAL-ER (52731)" "2014:154:17:10:41.320"   1.0 Nothing Nothing  255.1291  40.9430 176.16 116.63 111.74   
 , toR Nothing     (SpecialObs "G3_47") Nothing       "CAL-ER (52730)" "2014:154:17:57:05.398"   3.0 Nothing Nothing   74.0000  75.0000 357.02  52.68  64.06   
 , toR Nothing     (SpecialObs "T_X48") Nothing       "CAL-ER (52729)" "2014:154:19:47:57.325"   0.1 Nothing Nothing  151.3000  31.5000 255.34  70.20  64.42   
 , toR (Just 901116)    (ObsId 16616) (Just 0)         "30 Doradus" "2014:154:22:59:13.248"  35.0 (Just ACISI) (Just NONE)  84.7063 (-69.0842) 201.85  91.92 112.38
 , toR (Just 401587)    (ObsId 15790) (Just 0)    "IGR J15335-5420" "2014:155:09:10:44.942"   5.0 (Just ACISI) (Just NONE) 233.3392 (-54.3595) 328.74 145.04  57.13
 , toR (Just 901100)    (ObsId 16160) (Just 0)          "4C -00.62" "2014:155:11:17:54.093"  63.0 (Just ACISI) (Just NONE) 240.3061  (-0.4712) 212.00 155.21 127.44
 , toR (Just 702960)    (ObsId 16066) (Just 0)            "3C293.1" "2014:156:05:13:11.093"  12.0 (Just ACISS) (Just NONE) 208.6765  16.2713 229.42 121.87  37.62
 , toR (Just 801409)    (ObsId 16534) (Just 0)  "Centaurus cluster" "2014:156:09:05:02.197"  56.1 (Just ACISS) (Just NONE) 192.1895 (-41.3113) 270.66 126.14  73.58
 , toR Nothing     (SpecialObs "T_E48") Nothing       "CAL-ER (52727)" "2014:157:01:42:23.998"   0.1 Nothing Nothing  305.0000  (-3.0000) 116.15 126.73 157.96   
 , toR Nothing     (SpecialObs "GG_48") Nothing       "CAL-ER (52726)" "2014:157:02:20:48.558"   4.3 Nothing Nothing  280.0609 (-14.1395) 112.66 153.77  27.02   
 , toR Nothing     (SpecialObs "G1_48") Nothing       "CAL-ER (52725)" "2014:157:05:09:58.991"   1.0 Nothing Nothing  247.0000   2.0000 195.06 154.46  92.04   
 , toR Nothing     (SpecialObs "G2_48") Nothing       "CAL-ER (52724)" "2014:157:05:51:01.052"   8.0 Nothing Nothing  224.1527  34.8886 210.69 115.89  39.94   
 , toR Nothing     (SpecialObs "G3_48") Nothing       "CAL-ER (52723)" "2014:157:08:30:49.632"   1.0 Nothing Nothing  168.5000  25.0000 247.64  84.32  49.13   
 , toR Nothing     (SpecialObs "T_X49") Nothing       "CAL-ER (52722)" "2014:157:10:27:58.804"   0.1 Nothing Nothing  168.5000  25.0000 247.68  84.24   0.04   
 , toR (Just 901116)    (ObsId 16197) (Just 0)         "30 Doradus" "2014:157:13:05:14.804"  68.7 (Just ACISI) (Just NONE)  84.7063 (-69.0842) 201.85  92.03 111.22
 , toR (Just 601112)    (ObsId 16024) (Just 1)                "M83" "2014:158:08:44:05.154"  30.0 (Just ACISI) (Just NONE) 204.2366 (-29.8521) 266.40 134.13  82.53
 , toR (Just 901100)    (ObsId 16619) (Just 0)          "4C -00.62" "2014:158:17:36:45.631"  37.0 (Just ACISI) (Just NONE) 240.3061  (-0.4712) 212.00 153.26  77.29
 , toR (Just 702958)    (ObsId 16064) (Just 0)            "3C288.1" "2014:159:04:23:46.723"  12.0 (Just ACISS) (Just NONE) 205.5699  60.3855 225.34  87.34  66.83
 , toR (Just 702755)    (ObsId 14518) (Just 1)         "Q2237+0305" "2014:159:08:23:06.084"  29.9 (Just ACISS) (Just NONE) 340.1454   3.3425 113.17  94.25 107.20
 , toR Nothing     (SpecialObs "T_E49") Nothing       "CAL-ER (52720)" "2014:159:17:48:48.557"   0.1 Nothing Nothing  283.4306 (-11.8645) 116.89 152.18  58.93   

 ]

