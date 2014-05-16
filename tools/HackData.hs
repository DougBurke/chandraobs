-- | An example data set taken from the Chandra Short-Term schedule and
--   manually converted.
--
module HackData (testSchedule) where

import Data.Time (UTCTime, readTime)

import System.Locale (defaultTimeLocale)

import Types
import PersistentTypes

-- Convert values like "2014:132:03:08:49.668"
-- to a time. This is 
--    year number : day number : hh :: mm :: ss.sss
-- in UTC (I guess)
--
-- for now assume all inputs are valid. Hopefully the DOY
-- values are all zero-padded to three characters and
-- start at 1
--
toTime :: String -> UTCTime
toTime =  readTime defaultTimeLocale "%Y:%j:%T%Q"

testSchedule :: [Record]
testSchedule = [
 Record (Just 501802) (ObsId 14481) (Just 1) "Cassiopeia A" (toTime "2014:132:03:08:49.668") 50.0 (Just ACISS) (Just NONE) 350.8962  58.8246  74.97  58.22  73.46

 , Record (Just 801422) (ObsId 16280) (Just 0) "G249.87-39.86" (toTime "2014:132:17:54:22.746") 7.0 (Just ACISI) (Just NONE) 72.4564 (-44.6653) 204.13  66.28 165.23

 , Record (Just 901116) (ObsId 16194) (Just 0) "30 Doradus" (toTime "2014:132:20:19:02.746")  31.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.84  25.34

 , Record Nothing (SpecialObs "T_E39") Nothing "CAL-ER (52778)" (toTime "2014:133:05:50:04.996")   0.1 Nothing Nothing 219.8433 (-42.5680) 337.85 154.31  63.59

 , Record Nothing (SpecialObs "GG_39") Nothing "CAL-ER (52777)" (toTime "2014:133:08:03:00.000")   0.1 Nothing Nothing 236.0147   6.4846 166.37 154.45 177.31

 , Record Nothing (SpecialObs "G1_39") Nothing "CAL-ER (52776)" (toTime "2014:133:12:52:26.688")   0.3 Nothing Nothing 19.0000  60.0000  41.91  47.19 107.40

 , Record Nothing (SpecialObs "T_X40") Nothing "CAL-ER (52775)" (toTime "2014:133:15:16:38.193")   0.1 Nothing Nothing 224.0000   6.0000 193.94 154.84 111.53

 , Record (Just 502237) (ObsId 16004) (Just 0) "Kepler's Supernova R" (toTime "2014:133:18:20:16.193") 104.0 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 149.48 113.86

 , Record (Just 702754) (ObsId 14517) (Just 1) "Q2237+0305" (toTime "2014:134:23:46:47.815")  30.0 (Just ACISS) (Just NONE) 340.1442   3.3410 108.61  71.29  79.62

 , Record (Just 901116) (ObsId 16615) (Just 0) "30 Doradus" (toTime "2014:135:08:44:31.003")  45.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.97  99.99

 , Record Nothing (SpecialObs "T_E40") Nothing "CAL-ER (52773)" (toTime "2014:135:22:09:14.231")   0.1 Nothing Nothing 220.1000 (-42.1000) 331.99 154.60  64.54

 , Record Nothing (SpecialObs "GG_40") Nothing "CAL-ER (52772)" (toTime "2014:136:00:22:00.000")   0.3 Nothing Nothing 67.0000 (-26.0000) 198.83  47.13 107.51

 , Record Nothing (SpecialObs "G1_40") Nothing "CAL-ER (52771)" (toTime "2014:136:01:43:19.088")   0.8 Nothing Nothing 105.0000  11.0000 254.38  51.09  71.19

 , Record Nothing (SpecialObs "G2_40") Nothing "CAL-ER (52770)" (toTime "2014:136:04:29:42.928")   0.5 Nothing Nothing  62.0271 (-29.2154) 191.69  49.08  78.13

 , Record Nothing (SpecialObs "T_X41") Nothing "CAL-ER (52769)" (toTime "2014:136:06:59:34.670")   0.1 Nothing Nothing 0.7666   8.5993 107.55  51.42  96.58

 , Record (Just 502237) (ObsId 16614) (Just 0) "Kepler's Supernova R" (toTime "2014:136:09:03:12.659")  36.8 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 152.00 100.61

 , Record (Just 601116) (ObsId 16028) (Just 1) "NGC 300" (toTime "2014:136:19:53:38.301")  65.0 (Just ACISI) (Just NONE)  13.7470 (-37.6893) 130.01  67.94  97.17

 , Record (Just 801381) (ObsId 16525) (Just 0) "Abell S0295" (toTime "2014:137:14:24:15.301")  45.0 (Just ACISI) (Just NONE)  41.3955 (-53.0284) 167.50  73.16  29.99

  -- TODO: I think the SpecialObs value here is an error in the input 
 , Record Nothing (SpecialObs "I5190") Nothing "CAL-ER (52768)" (toTime "2014:138:03:36:44.209")   0.6 Nothing Nothing 190.2715  (-5.1604) 247.67 134.54 121.42

 , Record (Just 801315) (ObsId 15190) (Just 0) "A1612" (toTime "2014:138:04:14:01.209")  29.1 (Just ACISI) (Just NONE) 191.9626  (-2.7668) 229.62 135.10  18.40

 , Record Nothing (SpecialObs "T_E41") Nothing "CAL-ER (52766)" (toTime "2014:138:13:12:14.645")   0.1 Nothing Nothing 219.7761 (-42.5475) 334.00 153.64 101.50

 -- this has lost the P1 identifier from 502218 / 15985

 , Record Nothing   (SpecialObs "GG_41") Nothing     "CAL-ER (52765)" (toTime "2014:138:16:08:30.968")  10.0 Nothing Nothing 237.0000  10.0000 176.47 150.36 164.87
 , Record Nothing   (SpecialObs "G1_41") Nothing     "CAL-ER (52764)" (toTime "2014:138:19:03:40.968")  10.0 Nothing Nothing 237.0000  10.0000 176.74 150.33   0.26
 , Record Nothing   (SpecialObs "T_X42") Nothing     "CAL-ER (52763)" (toTime "2014:138:22:58:55.645")   0.1 Nothing Nothing 258.0000 (-45.0000)  43.72 148.52 145.40
 , Record (Just 601090)    (ObsId 15384) (Just 0)             "ngc6744" (toTime "2014:139:01:19:16.645")  53.5 (Just ACISI) (Just NONE) 287.4564 (-63.8721)  67.01 123.91  25.07
 , Record (Just 702973)    (ObsId 16082) (Just 0)            "3C 220.3" (toTime "2014:139:17:03:11.601")  21.0 (Just ACISS) (Just NONE) 144.7309  83.2744 280.12  70.20 165.37
 , Record (Just 502218)    (ObsId 15985) (Just 0)      "PSR J2307+2225" (toTime "2014:139:23:25:56.652")   3.5 (Just ACISS) (Just NONE) 346.9388  22.4116 102.03  64.31  77.63
 , Record (Just 601126)    (ObsId 16214) (Just 3)              "Sgr A*" (toTime "2014:140:01:00:39.977")  50.0 (Just ACISS) (Just NONE) 266.4253 (-29.0313)  80.00 151.37  94.00
 , Record (Just 801381)    (ObsId 16524) (Just 0)         "Abell S0295" (toTime "2014:140:15:31:09.620")  43.8 (Just ACISI) (Just NONE)  41.3979 (-53.0335) 152.03  74.27  97.47
 , Record Nothing   (SpecialObs "T_E42") Nothing     "CAL-ER (52761)" (toTime "2014:141:04:25:19.493")   0.1 Nothing Nothing 244.0000 (-50.0000)  11.91 149.68  75.93
 , Record Nothing   (SpecialObs "GG_42") Nothing     "CAL-ER (52760)" (toTime "2014:141:07:14:35.865")  10.0 Nothing Nothing 270.0000 (-27.0000)  83.86 149.77  62.49
 , Record Nothing   (SpecialObs "G1_42") Nothing     "CAL-ER (52759)" (toTime "2014:141:10:36:51.599")   0.1 Nothing Nothing 224.0000 (-47.0000) 332.39 150.88  90.38
 , Record Nothing   (SpecialObs "G2_42") Nothing     "CAL-ER (52758)" (toTime "2014:141:11:16:55.400")   3.0 Nothing Nothing  95.5000 (-15.0000) 227.43  50.98 103.04
 , Record Nothing   (SpecialObs "G3_42") Nothing     "CAL-ER (52757)" (toTime "2014:141:12:45:19.039")   3.0 Nothing Nothing 224.0000 (-47.0000) 332.19 150.85 103.02
 , Record Nothing   (SpecialObs "T_X43") Nothing     "CAL-ER (52756)" (toTime "2014:141:13:53:30.726")   0.1 Nothing Nothing 233.0000 (-54.0000) 351.45 145.97  15.24
 , Record (Just 901099)    (ObsId 16159) (Just 0)       "CL J1449+0856" (toTime "2014:141:16:38:46.130")  96.0 (Just ACISS) (Just NONE) 222.3214   8.9591 208.04 146.91 145.04
 , Record (Just 401442)    (ObsId 14609) (Just 2)                 "M22" (toTime "2014:142:20:03:50.934")  85.9 (Just ACISS) (Just NONE) 279.1145 (-23.9246)  92.54 143.10 133.11
 , Record Nothing   (SpecialObs "T_E43") Nothing     "CAL-ER (52754)" (toTime "2014:143:20:43:44.271")   0.1 Nothing Nothing  22.0000 (-19.0000) 134.59  54.60  93.70
 , Record Nothing   (SpecialObs "T_X44") Nothing     "CAL-ER (52753)" (toTime "2014:143:22:00:00.000")   0.1 Nothing Nothing 277.0001 (-85.0000)  37.86 114.63  72.50
 , Record (Just 901116)    (ObsId 16195) (Just 0)          "30 Doradus" (toTime "2014:144:14:28:00.000")  45.0 (Just ACISI) (Just NONE)  84.7062 (-69.0842) 201.98  91.43  26.04
 , Record (Just 401568)    (ObsId 15771) (Just 2)     "Holmberg II X-1" (toTime "2014:145:03:47:33.062")  13.0 (Just ACISS) (Just NONE) 124.8168  70.7224 288.60  61.63 153.23

 ]
