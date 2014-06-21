-- | An example data set taken from the Chandra Short-Term schedule and
--   manually converted.
--
module HackData (testSchedule, testNonScience) where

import Data.Maybe (isJust, mapMaybe)

import Types

data ObsName = SpecialObs String | ObsId ObsIdVal deriving Eq

toOI :: Int -> ObsName
toOI = ObsId . ObsIdVal

toSI :: STS -> ScheduleItem
toSI (STS mSeqNum obs _ tgt stTime eval _ _ _ _ _ _ _) =
  let t1 = toCTime stTime
      t2 = endCTime t1 tks
      tks = TimeKS eval
      obsid = case obs of
               ObsId oi -> oi
               SpecialObs _ -> getObsIdValue tgt
  in ScheduleItem {
    siObsId = obsid
    , siScienceObs = isJust mSeqNum
    , siStart = t1
    , siEnd = t2
    , siDuration = tks
    }

-- NOTE: the patterns below are not exhaustive, since if they don't match
--       there's an error in the input data and I want it to error out
--       at compile time. This is all temporary code!

{-
toSO :: STS -> Maybe ScienceObs
toSO (STS Nothing _ _ _ _ _ _ _ _ _ _ _ _) = Nothing
toSO (STS (Just seqNum) (ObsId obs) _ tgt stTime eval (Just inst) (Just grat) ra dec roll pitch slew) =
  let t1 = toCTime stTime
      tks = TimeKS eval
      raV = RA ra
      decV = Dec dec
  in Just $ ScienceObs (Sequence seqNum) obs tgt t1 tks inst grat raV decV roll pitch slew -- []
-}

toNS :: STS -> Maybe NonScienceObs
toNS (STS (Just _) _ _ _ _ _ _ _ _ _ _ _ _) = Nothing
toNS (STS Nothing (SpecialObs obsname) _ tgt stTime eval Nothing Nothing ra dec roll _ _) =
  let t1 = toCTime stTime
      tks = TimeKS eval
      raV = RA ra
      decV = Dec dec
      obsid = getObsIdValue tgt
  in Just $ NonScienceObs obsname obsid tgt t1 tks raV decV roll 

-- Assume the name is "CAL-ER (xxx)" and xxx is a valid Int
getObsIdValue :: String -> ObsIdVal
getObsIdValue tname =
  let ostr = takeWhile (/= ')') $ drop 1 $ dropWhile (/= '(') tname
  in ObsIdVal $ read ostr

-- Since we cut/paste from the web site, there's a lot of repeated info here
testSchedule :: [ScheduleItem]
testSchedule = map toSI sts

{-
testScience :: [ScienceObs]
testScience = mapMaybe toSO sts
-}

testNonScience :: [NonScienceObs]
testNonScience = mapMaybe toNS sts

--
-- data from the short-term schedule page
--
data STS = STS 
  (Maybe Int)
  ObsName
  (Maybe Int)  -- number of constraints; probably ignoring this field
  String     -- target name
  String     -- start time (passed to toCTime)
  Double     -- exposure time
  (Maybe Instrument)
  (Maybe Grating)
  Double     -- ra
  Double     -- dec
  Double     -- roll
  Double     -- pitch
  Double     -- slew

sts :: [STS]
sts = [
 STS (Just 501802) (toOI 14481) (Just 1) "Cassiopeia A" "2014:132:03:08:49.668" 50.0 (Just ACISS) (Just NONE) 350.8962  58.8246  74.97  58.22  73.46

 , STS (Just 801422) (toOI 16280) (Just 0) "G249.87-39.86" "2014:132:17:54:22.746" 7.0 (Just ACISI) (Just NONE) 72.4564 (-44.6653) 204.13  66.28 165.23

 , STS (Just 901116) (toOI 16194) (Just 0) "30 Doradus" "2014:132:20:19:02.746"  31.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.84  25.34

 , STS Nothing (SpecialObs "T_E39") Nothing "CAL-ER (52778)" "2014:133:05:50:04.996"   0.1 Nothing Nothing 219.8433 (-42.5680) 337.85 154.31  63.59

 , STS Nothing (SpecialObs "GG_39") Nothing "CAL-ER (52777)" "2014:133:08:03:00.000"   0.1 Nothing Nothing 236.0147   6.4846 166.37 154.45 177.31

 , STS Nothing (SpecialObs "G1_39") Nothing "CAL-ER (52776)" "2014:133:12:52:26.688"   0.3 Nothing Nothing 19.0000  60.0000  41.91  47.19 107.40

 , STS Nothing (SpecialObs "T_X40") Nothing "CAL-ER (52775)" "2014:133:15:16:38.193"   0.1 Nothing Nothing 224.0000   6.0000 193.94 154.84 111.53

 , STS (Just 502237) (toOI 16004) (Just 0) "Kepler's Supernova R" "2014:133:18:20:16.193" 104.0 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 149.48 113.86

 , STS (Just 702754) (toOI 14517) (Just 1) "Q2237+0305" "2014:134:23:46:47.815"  30.0 (Just ACISS) (Just NONE) 340.1442   3.3410 108.61  71.29  79.62

 , STS (Just 901116) (toOI 16615) (Just 0) "30 Doradus" "2014:135:08:44:31.003"  45.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.97  99.99

 , STS Nothing (SpecialObs "T_E40") Nothing "CAL-ER (52773)" "2014:135:22:09:14.231"   0.1 Nothing Nothing 220.1000 (-42.1000) 331.99 154.60  64.54

 , STS Nothing (SpecialObs "GG_40") Nothing "CAL-ER (52772)" "2014:136:00:22:00.000"   0.3 Nothing Nothing 67.0000 (-26.0000) 198.83  47.13 107.51

 , STS Nothing (SpecialObs "G1_40") Nothing "CAL-ER (52771)" "2014:136:01:43:19.088"   0.8 Nothing Nothing 105.0000  11.0000 254.38  51.09  71.19

 , STS Nothing (SpecialObs "G2_40") Nothing "CAL-ER (52770)" "2014:136:04:29:42.928"   0.5 Nothing Nothing  62.0271 (-29.2154) 191.69  49.08  78.13

 , STS Nothing (SpecialObs "T_X41") Nothing "CAL-ER (52769)" "2014:136:06:59:34.670"   0.1 Nothing Nothing 0.7666   8.5993 107.55  51.42  96.58

 , STS (Just 502237) (toOI 16614) (Just 0) "Kepler's Supernova R" "2014:136:09:03:12.659"  36.8 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 152.00 100.61

 , STS (Just 601116) (toOI 16028) (Just 1) "NGC 300" "2014:136:19:53:38.301"  65.0 (Just ACISI) (Just NONE)  13.7470 (-37.6893) 130.01  67.94  97.17

 , STS (Just 801381) (toOI 16525) (Just 0) "Abell S0295" "2014:137:14:24:15.301"  45.0 (Just ACISI) (Just NONE)  41.3955 (-53.0284) 167.50  73.16  29.99

  -- TODO: I think the SpecialObs value here is an error in the input 
 , STS Nothing (SpecialObs "I5190") Nothing "CAL-ER (52768)" "2014:138:03:36:44.209"   0.6 Nothing Nothing 190.2715  (-5.1604) 247.67 134.54 121.42


 , STS (Just 801315) (toOI 15190) (Just 0) "A1612" "2014:138:04:14:01.209"  29.1 (Just ACISI) (Just NONE) 191.9626  (-2.7668) 229.62 135.10  18.40

 , STS Nothing (SpecialObs "T_E41") Nothing "CAL-ER (52766)" "2014:138:13:12:14.645"   0.1 Nothing Nothing 219.7761 (-42.5475) 334.00 153.64 101.50

 , STS Nothing (SpecialObs "GG_41") Nothing "CAL-ER (52765)" "2014:138:16:08:30.968"  10.0 Nothing Nothing  237.0000  10.0000 176.47 150.36 164.87   
 , STS Nothing (SpecialObs "G1_41") Nothing "CAL-ER (52764)" "2014:138:19:03:40.968"  10.0 Nothing Nothing  237.0000  10.0000 176.74 150.33   0.26   
 , STS Nothing (SpecialObs "T_X42") Nothing "CAL-ER (52763)" "2014:138:22:58:55.645"   0.1 Nothing Nothing  258.0000 (-45.0000)  43.72 148.52 145.40   
 , STS (Just 601090) (toOI 15384) (Just 0) "ngc6744" "2014:139:01:19:16.645"  53.5 (Just ACISI) (Just NONE) 287.4564 (-63.8721)  67.01 123.91  25.07
 , STS (Just 502105) (toOI 15872) (Just 0) "Short GRB ToO" "2014:139:16:57:15.388"  20.0 (Just ACISS) (Just NONE) 253.0210  39.9670 161.96 118.24 138.66
 , STS (Just 502218) (toOI 15985) (Just 0) "PSR J2307+2225" "2014:139:23:03:34.425"   3.5 (Just ACISS) (Just NONE) 346.9388  22.4116 102.02  64.30  78.68
 , STS (Just 601126) (toOI 16214) (Just 3) "Sgr A*" "2014:140:00:38:17.736"  50.0 (Just ACISS) (Just NONE) 266.4253 (-29.0313)  80.00 151.35  94.00
 , STS (Just 801381) (toOI 16524) (Just 0) "Abell S0295" "2014:140:15:08:47.380"  45.1 (Just ACISI) (Just NONE)  41.3979 (-53.0335) 152.03  74.26  97.47
 , STS Nothing (SpecialObs "T_E42") Nothing "CAL-ER (52761)" "2014:141:04:25:19.493"   0.1 Nothing Nothing  244.0000 (-50.0000)  11.91 149.68  75.93   
 , STS Nothing (SpecialObs "GG_42") Nothing "CAL-ER (52760)" "2014:141:07:14:35.865"  10.0 Nothing Nothing  270.0000 (-27.0000)  83.86 149.77  62.49   
 , STS Nothing (SpecialObs "G1_42") Nothing "CAL-ER (52759)" "2014:141:10:36:51.599"   0.1 Nothing Nothing  224.0000 (-47.0000) 332.39 150.88  90.38   
 , STS Nothing (SpecialObs "G2_42") Nothing "CAL-ER (52758)" "2014:141:11:16:55.400"   3.0 Nothing Nothing   95.5000 (-15.0000) 227.43  50.98 103.04   
 , STS Nothing (SpecialObs "G3_42") Nothing "CAL-ER (52757)" "2014:141:12:45:19.039"   3.0 Nothing Nothing  224.0000 (-47.0000) 332.19 150.85 103.02   
 , STS Nothing (SpecialObs "T_X43") Nothing "CAL-ER (52756)" "2014:141:13:53:30.726"   0.1 Nothing Nothing  233.0000 (-54.0000) 351.45 145.97  15.24   
 , STS (Just 901099) (toOI 16159) (Just 0) "CL J1449+0856" "2014:141:16:38:46.130"  96.0 (Just ACISS) (Just NONE) 222.3214   8.9591 208.04 146.91 145.04
 , STS (Just 401442) (toOI 14609) (Just 2) "M22" "2014:142:20:03:50.934"  85.9 (Just ACISS) (Just NONE) 279.1145 (-23.9246)  92.54 143.10 133.11
 , STS Nothing (SpecialObs "T_E43") Nothing "CAL-ER (52754)" "2014:143:20:43:44.271"   0.1 Nothing Nothing   22.0000 (-19.0000) 134.59  54.60  93.70   
 , STS Nothing (SpecialObs "T_X44") Nothing "CAL-ER (52753)" "2014:143:22:00:00.000"   0.1 Nothing Nothing  277.0001 (-85.0000)  37.86 114.63  72.50   
 , STS (Just 901116) (toOI 16195) (Just 0) "30 Doradus" "2014:144:14:28:00.000"  45.0 (Just ACISI) (Just NONE)  84.7062 (-69.0842) 201.98  91.43  26.04
 , STS (Just 401568) (toOI 15771) (Just 2) "Holmberg II X-1" "2014:145:03:47:33.062"  13.0 (Just ACISS) (Just NONE) 124.8168  70.7224 288.60  61.63 153.23
 , STS (Just 502271) (toOI 16618) (Just 0) "GRB140515A" "2014:145:07:54:52.554"  20.0 (Just ACISS) (Just NONE) 186.0587  15.1260 256.00 114.47  68.21
 , STS (Just 501948) (toOI 16489) (Just 0) "HESS J1809-193" "2014:145:14:19:24.593"  65.7 (Just ACISI) (Just NONE) 272.4377 (-19.2951) 108.00 151.51 160.65
 , STS (Just 601138) (toOI 16294) (Just 1) "M31 BHXNe" "2014:146:09:13:17.508"   5.0 (Just ACISI) (Just NONE)  10.6971  41.2515  81.74  48.15 108.67
 , STS Nothing (SpecialObs "T_E44") Nothing "CAL-ER (52750)" "2014:146:11:30:04.215"   0.1 Nothing Nothing  262.8282  (-0.9587) 137.49 152.06 104.00   
 , STS Nothing (SpecialObs "GG_44") Nothing "CAL-ER (52749)" "2014:146:13:10:09.653"   0.1 Nothing Nothing  262.8282  (-0.9587) 137.61 152.10   0.12   
 , STS Nothing (SpecialObs "G1_44") Nothing "CAL-ER (52748)" "2014:146:17:26:04.176"   0.5 Nothing Nothing  219.0000  (-6.0000) 234.80 152.04 106.83   
 , STS Nothing (SpecialObs "T_X45") Nothing "CAL-ER (52747)" "2014:146:21:24:46.655"   0.1 Nothing Nothing  260.0000 (-48.0000)  32.17 150.06 141.77   
 , STS (Just 801409) (toOI 16223) (Just 0) "Centaurus cluster" "2014:146:23:49:18.215" 181.3 (Just ACISS) (Just NONE) 192.1895 (-41.3162) 269.99 132.53  83.90
 , STS Nothing (SpecialObs "T_E45") Nothing "CAL-ER (52745)" "2014:149:02:42:39.112"   0.1 Nothing Nothing  219.7414 (-29.5202) 282.86 155.35  25.43   
 , STS Nothing (SpecialObs "GG_45") Nothing "CAL-ER (52744)" "2014:149:05:29:25.804"   1.3 Nothing Nothing  266.0000   0.0000 138.84 150.77 159.33   
 , STS Nothing (SpecialObs "G1_45") Nothing "CAL-ER (52743)" "2014:149:09:49:55.071"   0.7 Nothing Nothing  220.0640 (-16.3622) 253.85 154.96 127.44   
 , STS Nothing (SpecialObs "T_X46") Nothing "CAL-ER (52742)" "2014:149:12:13:32.068"   0.1 Nothing Nothing  232.0000  (-1.0000) 212.72 155.23  46.92   
 , STS (Just 502223) (toOI 15990) (Just 1) "Swift J1822.3-1606" "2014:149:14:07:22.697"  14.0 (Just ACISS) (Just NONE) 275.5899 (-16.0941)  96.50 151.67 127.87
 , STS (Just 702724) (toOI 14487) (Just 1) "QJ0158-4325" "2014:149:18:38:07.037"  19.0 (Just ACISS) (Just NONE)  29.7062 (-43.4211) 144.59  73.25  98.58
 , STS (Just 901116) (toOI 16196) (Just 0) "30 Doradus" "2014:150:00:22:47.037"  68.0 (Just ACISI) (Just NONE)  84.7062 (-69.0842) 202.00  91.68  38.88
 , STS (Just 901105) (toOI 16165) (Just 0) "1RXS J174559.6-37005" "2014:150:19:48:26.306"   4.0 (Just ACISS) (Just NONE) 266.4876 (-37.0384)  41.99 157.53  75.70
 , STS (Just 901117) (toOI 16286) (Just 0) "ESO512-009" "2014:150:21:33:29.558"  13.0 (Just ACISS) (Just NONE) 219.8258 (-25.4505) 279.99 154.43 102.99
 , STS (Just 901116) (toOI 16617) (Just 0) "30 Doradus" "2014:151:01:45:49.205"  59.6 (Just ACISI) (Just NONE)  84.7062 (-69.0842) 202.00  91.73  90.72
 , STS Nothing (SpecialObs "T_E46") Nothing "CAL-ER (52740)" "2014:151:19:00:26.007"   0.1 Nothing Nothing  244.2000 (-51.0000) 352.05 150.76  59.31   
 , STS Nothing (SpecialObs "GG_46") Nothing "CAL-ER (52739)" "2014:151:20:59:05.211"   1.6 Nothing Nothing  264.5513  (-3.5085) 141.15 155.84 142.92   
 , STS Nothing (SpecialObs "T_X47") Nothing "CAL-ER (52738)" "2014:152:02:09:26.364"   0.1 Nothing Nothing  222.6617 (-15.5862) 251.42 154.54 122.33   
 , STS (Just 502102) (toOI 15869) (Just 0) "SN 2010jl" "2014:152:06:24:29.221"  40.0 (Just ACISS) (Just NONE) 145.7188   9.5194 249.67  74.29  80.25

 , STS (Just 801377)    (toOI 16123) (Just 0) "RXJ1416.4+2315 Offse" "2014:152:18:01:31.984"   5.0 (Just ACISI) (Just NONE) 214.1596  23.4419 219.63 123.03  66.96
 , STS (Just 702978)    (toOI 16088) (Just 0) "SDSSJ162210.11+07021" "2014:152:19:50:08.984"  65.8 (Just ACISS) (Just NONE) 245.5584   7.0526 194.00 150.64  38.21
 , STS (Just 702737)    (toOI 14500) (Just 1)      "SDSS1004+4112" "2014:153:14:43:02.164"  25.0 (Just ACISS) (Just NONE) 151.1333  41.2359 258.45  68.95  89.64
 , STS (Just 702990)    (toOI 16102) (Just 0)   "J011341.1+010608" "2014:153:22:30:21.065"  15.0 (Just ACISS) (Just NONE)  18.4413   1.0879 116.60  54.90 158.17
 , STS (Just 601122)    (toOI 16210) (Just 3)             "Sgr A*" "2014:154:03:20:13.610"  18.7 (Just ACISS) (Just NONE) 266.4234 (-29.0318)  76.00 164.39 109.69
 , STS Nothing     (SpecialObs "I6561") Nothing       "CAL-ER (52736)" "2014:154:08:51:04.836"   0.3 Nothing Nothing  289.0000 (-26.0000)  92.11 145.28  20.96   
 , STS Nothing     (SpecialObs "T_E47") Nothing       "CAL-ER (52734)" "2014:154:10:03:16.248"   0.1 Nothing Nothing  289.0000 (-26.0000)  92.10 145.33   0.01   
 , STS Nothing     (SpecialObs "GG_47") Nothing       "CAL-ER (52733)" "2014:154:11:35:08.830"   0.8 Nothing Nothing  280.0000 (-15.0000) 109.36 151.77  24.64   
 , STS Nothing     (SpecialObs "G1_47") Nothing       "CAL-ER (52732)" "2014:154:14:17:00.106"   8.0 Nothing Nothing  337.0000 (-27.5000) 108.30 103.68  58.62   
 , STS Nothing     (SpecialObs "G2_47") Nothing       "CAL-ER (52731)" "2014:154:17:10:41.320"   1.0 Nothing Nothing  255.1291  40.9430 176.16 116.63 111.74   
 , STS Nothing     (SpecialObs "G3_47") Nothing       "CAL-ER (52730)" "2014:154:17:57:05.398"   3.0 Nothing Nothing   74.0000  75.0000 357.02  52.68  64.06   
 , STS Nothing     (SpecialObs "T_X48") Nothing       "CAL-ER (52729)" "2014:154:19:47:57.325"   0.1 Nothing Nothing  151.3000  31.5000 255.34  70.20  64.42   
 , STS (Just 901116)    (toOI 16616) (Just 0)         "30 Doradus" "2014:154:22:59:13.248"  35.0 (Just ACISI) (Just NONE)  84.7063 (-69.0842) 201.85  91.92 112.38
 , STS (Just 401587)    (toOI 15790) (Just 0)    "IGR J15335-5420" "2014:155:09:10:44.942"   5.0 (Just ACISI) (Just NONE) 233.3392 (-54.3595) 328.74 145.04  57.13
 , STS (Just 901100)    (toOI 16160) (Just 0)          "4C -00.62" "2014:155:11:17:54.093"  63.0 (Just ACISI) (Just NONE) 240.3061  (-0.4712) 212.00 155.21 127.44
 , STS (Just 702960)    (toOI 16066) (Just 0)            "3C293.1" "2014:156:05:13:11.093"  12.0 (Just ACISS) (Just NONE) 208.6765  16.2713 229.42 121.87  37.62
 , STS (Just 801409)    (toOI 16534) (Just 0)  "Centaurus cluster" "2014:156:09:05:02.197"  56.1 (Just ACISS) (Just NONE) 192.1895 (-41.3113) 270.66 126.14  73.58
 , STS Nothing     (SpecialObs "T_E48") Nothing       "CAL-ER (52727)" "2014:157:01:42:23.998"   0.1 Nothing Nothing  305.0000  (-3.0000) 116.15 126.73 157.96   
 , STS Nothing     (SpecialObs "GG_48") Nothing       "CAL-ER (52726)" "2014:157:02:20:48.558"   4.3 Nothing Nothing  280.0609 (-14.1395) 112.66 153.77  27.02   
 , STS Nothing     (SpecialObs "G1_48") Nothing       "CAL-ER (52725)" "2014:157:05:09:58.991"   1.0 Nothing Nothing  247.0000   2.0000 195.06 154.46  92.04   
 , STS Nothing     (SpecialObs "G2_48") Nothing       "CAL-ER (52724)" "2014:157:05:51:01.052"   8.0 Nothing Nothing  224.1527  34.8886 210.69 115.89  39.94   
 , STS Nothing     (SpecialObs "G3_48") Nothing       "CAL-ER (52723)" "2014:157:08:30:49.632"   1.0 Nothing Nothing  168.5000  25.0000 247.64  84.32  49.13   
 , STS Nothing     (SpecialObs "T_X49") Nothing       "CAL-ER (52722)" "2014:157:10:27:58.804"   0.1 Nothing Nothing  168.5000  25.0000 247.68  84.24   0.04   
 , STS (Just 901116)    (toOI 16197) (Just 0)         "30 Doradus" "2014:157:13:05:14.804"  68.7 (Just ACISI) (Just NONE)  84.7063 (-69.0842) 201.85  92.03 111.22
 , STS (Just 601112)    (toOI 16024) (Just 1)                "M83" "2014:158:08:44:05.154"  30.0 (Just ACISI) (Just NONE) 204.2366 (-29.8521) 266.40 134.13  82.53
 , STS (Just 901100)    (toOI 16619) (Just 0)          "4C -00.62" "2014:158:17:36:45.631"  37.0 (Just ACISI) (Just NONE) 240.3061  (-0.4712) 212.00 153.26  77.29
 , STS (Just 702958)    (toOI 16064) (Just 0)            "3C288.1" "2014:159:04:23:46.723"  12.0 (Just ACISS) (Just NONE) 205.5699  60.3855 225.34  87.34  66.83
 , STS (Just 702755)    (toOI 14518) (Just 1)         "Q2237+0305" "2014:159:08:23:06.084"  29.9 (Just ACISS) (Just NONE) 340.1454   3.3425 113.17  94.25 107.20
 , STS Nothing     (SpecialObs "T_E49") Nothing       "CAL-ER (52720)" "2014:159:17:48:48.557"   0.1 Nothing Nothing  283.4306 (-11.8645) 116.89 152.18  58.93   

 , STS Nothing     (SpecialObs "GG_49") Nothing       "CAL-ER (52719)" "2014:159:20:16:01.468"   0.8   Nothing Nothing   27.3525  11.0509 110.86  48.41 105.86   
 , STS Nothing     (SpecialObs "T_X50") Nothing       "CAL-ER (52718)" "2014:160:02:37:31.823"   0.1   Nothing Nothing   72.1951 (-24.8814) 174.06  47.99  78.75   
 , STS (Just 801418)    (toOI 16237) (Just 0)   "MACSJ0416.1-2403" "2014:160:05:17:09.513"  37.0 (Just ACISI) (Just NONE)  64.0071 (-24.0584) 163.87  48.64  10.11
 , STS (Just 901115)    (toOI 16183) (Just 0) "Chandra Deep Field-S" "2014:160:15:59:06.513" 100.0 (Just ACISI) (Just NONE)  53.1394 (-27.8073) 144.51  55.92  17.94
 , STS (Just 702725)    (toOI 14488) (Just 1)        "QJ0158-4325" "2014:161:20:06:47.513"  19.0 (Just ACISS) (Just NONE)  29.7050 (-43.4251) 135.15  80.24  24.89
 , STS (Just 801426)    (toOI 16284) (Just 0)      "G118.60+28.55" "2014:162:02:11:25.494"   7.0 (Just ACISI) (Just NONE) 260.6417  85.9033 178.38  71.07 146.10
 , STS (Just 601113)    (toOI 16025) (Just 1)           "NGC 3310" "2014:162:04:32:31.837"  10.0 (Just ACISS) (Just NONE) 159.6765  53.5264 262.99  66.15  40.24
 , STS Nothing     (SpecialObs "T_E50") Nothing       "CAL-ER (52716)" "2014:162:08:05:58.908"   0.1   Nothing Nothing  249.4600  (-1.7910) 203.28 156.70  91.55   
 , STS Nothing     (SpecialObs "GG_50") Nothing       "CAL-ER (52715)" "2014:162:10:27:19.955"   0.1   Nothing Nothing  284.7000 (-32.9000)  72.70 155.65 144.86   
 , STS Nothing     (SpecialObs "G1_50") Nothing       "CAL-ER (52714)" "2014:162:11:22:18.840"   0.8   Nothing Nothing   37.0000 (-10.0000) 129.09  53.06 102.69   
 , STS Nothing     (SpecialObs "T_X51") Nothing       "CAL-ER (52713)" "2014:162:18:08:19.283"   0.1   Nothing Nothing  139.1002  28.0010 260.94  53.28 164.90   
 , STS (Just 901116)    (toOI 16198) (Just 0)         "30 Doradus" "2014:162:20:53:35.283"  40.0 (Just ACISI) (Just NONE)  84.7172 (-69.0874) 187.04  92.24 110.92
 , STS (Just 200937)    (toOI 15724) (Just 0)              "GJ581" "2014:163:08:41:51.186"  50.0 (Just ACISS) (Just NONE) 229.8577  (-7.6978) 247.00 146.93 117.44
 , STS (Just 801384)    (toOI 16613) (Just 0)              "3C320" "2014:163:23:04:54.269"  50.0 (Just ACISS) (Just NONE) 232.8770  35.5789 197.99 115.55  63.98
 , STS (Just 702951)    (toOI 16057) (Just 0)              "3C217" "2014:164:13:29:59.467"  12.0 (Just ACISS) (Just NONE) 137.1960  37.8271 270.55  49.35  73.14
 , STS (Just 702748)    (toOI 14511) (Just 1)       "RXJ1131-1231" "2014:164:17:19:15.312"  10.0 (Just ACISS) (Just NONE) 172.9627 (-12.5079) 247.53  96.00  61.94
 , STS (Just 801424)    (toOI 16282) (Just 0)      "G271.50-56.55" "2014:164:20:44:12.517"   9.1 (Just ACISI) (Just NONE)  41.3419 (-53.0131) 143.10  83.95 102.54
 , STS Nothing     (SpecialObs "T_E51") Nothing       "CAL-ER (52711)" "2014:165:00:08:32.419"   0.1   Nothing Nothing  280.0000 (-45.0000)  39.89 153.76  69.99   
 , STS Nothing     (SpecialObs "GG_51") Nothing       "CAL-ER (52710)" "2014:165:02:19:53.201"   0.7   Nothing Nothing  231.0962 (-34.5521) 284.40 150.72  89.89   
 , STS Nothing     (SpecialObs "G1_51") Nothing       "CAL-ER (52709)" "2014:165:04:52:12.503"   4.0   Nothing Nothing  280.0000 (-45.0000)  39.53 153.88  89.57   
 , STS Nothing     (SpecialObs "G2_51") Nothing       "CAL-ER (52708)" "2014:165:06:50:23.243"   0.5   Nothing Nothing  240.6476  (-3.4856) 224.79 151.18 159.44   
 , STS Nothing     (SpecialObs "T_X52") Nothing       "CAL-ER (52707)" "2014:165:09:01:16.394"   0.1   Nothing Nothing  289.0000 (-22.0000)  98.04 155.49 142.34   
 , STS (Just 702942)    (toOI 16048) (Just 0)               "3C44" "2014:165:11:09:46.419"  12.0 (Just ACISS) (Just NONE)  22.8601   6.3792 113.24  59.68  95.96
 , STS (Just 901116)    (toOI 16621) (Just 0)         "30 Doradus" "2014:165:15:05:25.056"  45.0 (Just ACISI) (Just NONE)  84.7172 (-69.0874) 187.04  92.34  90.65
 , STS (Just 702943)    (toOI 16049) (Just 0)               "3C54" "2014:166:04:20:47.408"  12.0 (Just ACISS) (Just NONE)  28.8891  43.7426  84.99  48.74 134.43
 , STS (Just 703008)    (toOI 16531) (Just 0)           "3C 390.3" "2014:166:08:11:28.008"  50.0 (Just ACISS) (Just HETG) 280.6601  79.7771 163.74  76.54  55.82
 , STS (Just 702944)    (toOI 16050) (Just 0)               "3C55" "2014:166:22:36:33.807"  12.0 (Just ACISS) (Just NONE)  29.3098  28.8401  96.87  49.07  73.18
 , STS (Just 702973)    (toOI 16520) (Just 0)           "3C 220.3" "2014:167:02:29:34.706"  45.2 (Just ACISS) (Just NONE) 144.6940  83.2697 296.74  63.53  78.82
 , STS Nothing     (SpecialObs "T_E52") Nothing       "CAL-ER (52705)" "2014:167:15:56:20.188"   0.1   Nothing Nothing  261.7505   2.1062 186.46 154.38  90.96   

 , STS Nothing (SpecialObs "GG_52") Nothing "CAL-ER (52704)" "2014:167:18:52:47.502"   0.0 Nothing Nothing 276.0000 (-45.7000)  25.70 155.92 167.81   
 , STS Nothing (SpecialObs "G1_52") Nothing "CAL-ER (52703)" "2014:167:20:47:24.584"   0.0 Nothing Nothing 293.2000 (-62.5700)  39.00 136.44  19.53   
 , STS Nothing (SpecialObs "G2_52") Nothing "CAL-ER (52702)" "2014:167:21:47:57.216"   0.0 Nothing Nothing 289.0000 (-15.0000) 114.00 155.93  89.28   
 , STS Nothing (SpecialObs "G3_52") Nothing "CAL-ER (52701)" "2014:167:22:39:28.394"   0.0 Nothing Nothing 276.0000 (-45.7000)  25.23 155.98  87.11   
 , STS Nothing (SpecialObs "G4_52") Nothing "CAL-ER (52700)" "2014:168:00:19:23.821"   0.0 Nothing Nothing  59.6500 (-16.1000) 147.06  46.69 110.08   
 , STS Nothing (SpecialObs "T_X53") Nothing "CAL-ER (52698)" "2014:168:02:32:17.179"   0.1 Nothing Nothing 117.9600 ( 65.5400) 317.41  47.10 163.04   
 , STS (Just 200938) (toOI 15725) (Just 0) "GJ1214" "2014:168:03:41:48.959"  32.0 (Just ACISS) (Just NONE) 258.8502 (  4.9768) 192.32 150.98 103.96
 , STS (Just 200949) (toOI 16532) (Just 0) "HD 206267" "2014:168:13:07:59.559"  47.7 (Just ACISS) (Just HETG) 324.7814 ( 57.4762) 127.80  85.40  76.21
 , STS (Just 703008) (toOI 16220) (Just 0) "3C 390.3" "2014:169:02:49:09.559"  50.0 (Just ACISS) (Just HETG) 280.6585 ( 79.7781) 166.47  76.57  26.29
 , STS (Just 702973) (toOI 16521) (Just 0) "3C 220.3" "2014:169:17:12:37.159"  45.0 (Just ACISS) (Just NONE) 144.6892 ( 83.2688) 299.51  63.17  15.96
 , STS Nothing (SpecialObs "T_E53") Nothing "CAL-ER (52696)" "2014:170:06:39:41.261"   0.1 Nothing Nothing 267.0000 (-13.0000) 182.58 169.58 106.54   
 , STS Nothing (SpecialObs "GG_53") Nothing "CAL-ER (52695)" "2014:170:08:10:23.234"   0.0 Nothing Nothing 271.0000 (-33.5000)  17.47 169.47 166.93   
 , STS Nothing (SpecialObs "G1_53") Nothing "CAL-ER (52694)" "2014:170:09:28:39.939"   0.0 Nothing Nothing 277.0000 (-46.5000)  21.30 155.71  13.78   
 , STS Nothing (SpecialObs "G2_53") Nothing "CAL-ER (52693)" "2014:170:13:03:27.849"   0.0 Nothing Nothing  59.9600 (-16.0300) 144.74  47.93 109.30   
 , STS Nothing (SpecialObs "T_X54") Nothing "CAL-ER (52692)" "2014:170:15:24:16.142"   0.1 Nothing Nothing 277.0000 (-46.5000)  20.69 155.78 109.33   
 , STS (Just 401581) (toOI 15784) (Just 2) "SXP 1062" "2014:170:19:22:04.955"  30.0 (Just ACISS) (Just NONE)  22.0075 (-73.5606) 119.23 106.00  49.81
 , STS (Just 702963) (toOI 16070) (Just 0) "PKS 1718-649" "2014:171:04:09:21.955"  16.1 (Just ACISS) (Just NONE) 260.9069 (-65.0249) 347.99 138.08  38.43
 , STS (Just 702971) (toOI 16078) (Just 0) "ESO138-G1" "2014:171:08:56:15.955"  50.0 (Just ACISS) (Just NONE) 252.7856 (-59.2360) 335.68 142.43   8.60
 , STS (Just 501781) (toOI 14204) (Just 1) "1E1207.4-5209" "2014:171:23:14:25.035"  33.0 (Just ACISS) (Just NONE) 182.4930 (-52.4174) 257.62 110.24  41.87
 , STS (Just 502010) (toOI 14886) (Just 1) "GRB 130427A" "2014:172:08:57:49.915"  35.0 (Just ACISS) (Just NONE) 173.1314 ( 27.7220) 251.62  73.88  80.62
 , STS (Just 291191) (toOI 16479) (Just 2) "Vega" "2014:172:19:15:28.297"   2.0 (Just  HRCI) (Just NONE) 279.2636 ( 38.7970) 170.55 117.20  84.63
 , STS (Just 291192) (toOI 16480) (Just 2) "Vega" "2014:172:19:57:24.545"   2.0 (Just  HRCS) (Just NONE) 279.2636 ( 38.7971) 170.60 117.20   0.05
 , STS (Just 291193) (toOI 16481) (Just 2) "Vega" "2014:172:20:39:14.545"   1.0 (Just  HRCS) (Just NONE) 279.0524 ( 38.8238) 170.85 117.20   0.20
 , STS (Just 291194) (toOI 16482) (Just 2) "Vega" "2014:172:21:04:24.545"   1.0 (Just  HRCS) (Just NONE) 278.7352 ( 38.8621) 171.20 117.20   0.29
 , STS (Just 291195) (toOI 16483) (Just 2) "Vega" "2014:172:21:34:26.118"   1.0 (Just  HRCS) (Just NONE) 279.7785 ( 38.6791) 163.76 117.26   6.84
 , STS Nothing (SpecialObs "T_E54") Nothing "CAL-ER (52689)" "2014:172:22:25:36.653"   0.1 Nothing Nothing 270.0000 (  2.0000) 180.58 154.57  39.92   
 , STS Nothing (SpecialObs "GG_54") Nothing "CAL-ER (52688)" "2014:172:23:28:04.724"   0.0 Nothing Nothing 277.0000 (-47.5000)  14.83 155.34 169.79   
 , STS Nothing (SpecialObs "G1_54") Nothing "CAL-ER (52687)" "2014:173:03:27:44.364"   0.0 Nothing Nothing  59.9200 (-16.0000) 142.13  49.49 108.69   
 , STS Nothing (SpecialObs "G2_54") Nothing "CAL-ER (52686)" "2014:173:04:31:21.783"   0.0 Nothing Nothing 349.1000 (-26.6000) 106.96 109.94  66.31   
 , STS Nothing (SpecialObs "G3_54") Nothing "CAL-ER (52685)" "2014:173:05:18:19.095"   0.0 Nothing Nothing  59.9300 (-16.0000) 142.05  49.53  66.31   
 , STS Nothing (SpecialObs "T_X55") Nothing "CAL-ER (52683)" "2014:173:08:45:39.496"   0.1 Nothing Nothing 277.0000 (-47.5000)  13.89 155.41 108.75   
 , STS (Just 901115) (toOI 16180) (Just 0) "Chandra Deep Field-S" "2014:173:10:39:14.653"  50.0 (Just ACISI) (Just NONE)  53.1391 (-27.8105) 134.91  62.81  95.18
 , STS (Just 702963) (toOI 16623) (Just 0) "PKS 1718-649" "2014:174:01:06:54.394"  33.4 (Just ACISS) (Just NONE) 260.9069 (-65.0249) 347.99 137.80  84.73

 , STS (Just 200912) (toOI 15642) (Just 2)             "GJ 436"   "2014:174:11:05:33.304"  19.0 (Just ACISS) (Just NONE) 175.5431  26.7299 251.28  74.40 119.17

 , STS (Just 702927) (toOI 15662) (Just 2)           "NGC 5548"   "2014:174:16:45:22.413"   5.0 (Just ACISS) (Just LETG) 214.5166  25.1793 233.74 106.03  34.94
 , STS (Just 401614) (toOI 16624) (Just 1)       "PSR B1259-63"   "2014:174:18:46:12.825"   5.0 (Just ACISS) (Just NONE) 195.6733 (-63.8166) 271.10 116.77  99.03
 , STS (Just 200825) (toOI 14233) (Just 1)          "ALPHA CEN"   "2014:174:20:25:13.652"   9.5 (Just  HRCI) (Just NONE) 219.8295 (-60.8211) 292.08 128.26  11.55
 , STS (Just 801251) (toOI 15122) (Just 0)      "G253.47-33.72"   "2014:174:23:33:59.544"  18.0 (Just ACISI) (Just NONE)  81.4127 (-47.2330) 169.37  71.31  67.19
 , STS (Just 601139) (toOI 16295) (Just 1)          "M31 BHXNe"   "2014:175:05:15:58.878"   5.0 (Just ACISI) (Just NONE)  10.7048  41.2560 103.21  69.00 119.20
 , STS (Just 601121) (toOI 16033) (Just 0)           "NGC 4551"   "2014:175:07:23:43.569"  21.3 (Just ACISS) (Just NONE) 188.9067  12.2886 245.86  90.67 130.10
 , STS Nothing (SpecialObs "T_E55") Nothing       "CAL-ER (52681)"   "2014:175:13:59:58.169"   0.1 Nothing Nothing 252.7000  (-3.2000) 222.43 151.82  67.34   
 , STS Nothing (SpecialObs "GG_55") Nothing       "CAL-ER (52680)"   "2014:175:15:20:53.454"   2.0 Nothing Nothing 294.3661  (-9.8716) 126.30 155.59 107.20   
 , STS Nothing (SpecialObs "G1_55") Nothing       "CAL-ER (52679)"   "2014:175:21:07:55.771"   1.0 Nothing Nothing 274.0000 (-50.0000) 1.32 153.39 119.30   
 , STS Nothing (SpecialObs "T_X56") Nothing       "CAL-ER (52677)"   "2014:176:01:09:04.458"   0.1 Nothing Nothing 229.0000 (-62.0000) 301.30 131.11  34.77   
 , STS (Just 703003) (toOI 16115) (Just 0) "SDSS J135429.05+1327" "2014:176:02:28:50.276"   9.5 (Just ACISS) (Just NONE) 208.6221  13.4906 240.19 106.56  89.99
 , STS (Just 490024) (toOI 16622) (Just 1)            "Cyg X-3"   "2014:176:05:43:27.924"  28.5 (Just ACISS) (Just HETG) 308.1354  40.9561 146.89 108.19  93.57
 , STS (Just 790264) (toOI 16424) (Just 3)             "MKN421"   "2014:176:14:14:49.794"  15.0 (Just ACISS) (Just HETG) 166.1032  38.2324 261.16  62.09  93.89
 , STS (Just 401615) (toOI 16625) (Just 1)       "PSR B1259-63"   "2014:176:19:03:36.188"   5.0 (Just ACISS) (Just NONE) 195.6747 (-63.8162) 269.23 115.80 104.73
 , STS (Just 790265) (toOI 16425) (Just 2)             "MKN421"   "2014:176:21:05:43.208"  10.0 (Just ACISS) (Just LETG) 166.1121  38.2791 261.35  61.87 104.78
 , STS (Just 790266) (toOI 16426) (Just 2)             "MKN421"   "2014:177:00:03:56.502"  10.0 (Just  HRCS) (Just LETG) 166.0973  38.2303 261.42  61.77   0.08
 , STS (Just 790267) (toOI 16427) (Just 2)             "MKN421"   "2014:177:03:10:51.502"  10.0 (Just ACISS) (Just LETG) 166.1119  38.2791 261.51  61.67   0.11
 , STS (Just 790268) (toOI 16428) (Just 2)             "MKN421"   "2014:177:06:09:04.796"  10.0 (Just  HRCS) (Just LETG) 166.0972  38.2302 261.57  61.57   0.07
 , STS (Just 790269) (toOI 16429) (Just 2)             "MKN421"   "2014:177:09:15:59.796"  10.0 (Just ACISS) (Just LETG) 166.1117  38.2791 261.66  61.47   0.11
 , STS (Just 790270) (toOI 16430) (Just 2)             "MKN421"   "2014:177:12:14:13.091"  10.0 (Just  HRCS) (Just LETG) 166.0972  38.2302 261.71  61.37   0.07
 , STS (Just 790271) (toOI 16431) (Just 2)             "MKN421"   "2014:177:15:30:42.691"  15.0 (Just ACISS) (Just HETG) 166.1029  38.2323 261.78  61.27   0.07
 , STS (Just 901116) (toOI 16200) (Just 0)         "30 Doradus"   "2014:177:20:24:40.523"  27.7 (Just ACISI) (Just NONE)  84.7263 (-69.0922) 170.23  92.73 128.09
 , STS Nothing (SpecialObs "T_E56") Nothing       "CAL-ER (52675)"   "2014:178:04:45:24.388"   0.1 Nothing Nothing  92.6000 (-25.0000) 176.14  48.42  44.34   
 , STS Nothing (SpecialObs "GG_56") Nothing       "CAL-ER (52674)"   "2014:178:09:10:08.246"   1.5 Nothing Nothing 169.0000  22.0000 253.10  66.65 109.91   
 , STS Nothing (SpecialObs "T_X57") Nothing       "CAL-ER (52673)"   "2014:178:13:55:54.273"   0.1 Nothing Nothing 253.5000  (-3.6000) 226.01 150.53  86.74   
 , STS (Just 401603) (toOI 16208) (Just 0)        "Serpens X-1"   "2014:178:17:52:32.723" 142.5 (Just ACISS) (Just HETG) 280.0277   5.0404 169.00 151.44  62.64
 , STS (Just 702967) (toOI 16074) (Just 0)     "SDSSJ0122+0100"   "2014:180:10:08:09.161"   5.0 (Just ACISS) (Just NONE)  20.5909   0.9880 101.00  78.10 112.98
 , STS (Just 401582) (toOI 15785) (Just 2)           "SXP 1062"   "2014:180:12:03:37.884"  29.7 (Just ACISI) (Just NONE)  21.9939 (-73.5588) 109.89 108.39  74.90
 , STS Nothing (SpecialObs "T_E57") Nothing       "CAL-ER (52670)"   "2014:180:21:07:45.668"   0.1 Nothing Nothing 140.2000  (-2.6000) 235.22  48.09  97.07   
 ]
