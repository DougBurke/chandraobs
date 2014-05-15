-- | An example data set taken from the Chandra Short-Term schedule and
--   manually converted.
--
module HackData (testSchedule) where

import Types
import PersistentTypes

testSchedule :: [Record]
testSchedule = [
 Record (Just 501802) (ObsId 14481) (Just 1) "Cassiopeia A" "2014:132:03:08:49.668" 50.0 (Just ACISS) (Just NONE) 350.8962  58.8246  74.97  58.22  73.46 (Just "http://asc.harvard.edu/targets/501802/501802.14481.soe.dss.gif") (Just "http://asc.harvard.edu/targets/501802/501802.14481.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/501802/501802.14481.soe.rass.gif")

 , Record (Just 801422) (ObsId 16280) (Just 0) "G249.87-39.86" "2014:132:17:54:22.746" 7.0 (Just ACISI) (Just NONE) 72.4564 (-44.6653) 204.13  66.28 165.23 (Just "http://asc.harvard.edu/targets/801422/801422.16280.soe.dss.gif") (Just "http://asc.harvard.edu/targets/801422/801422.16280.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/801422/801422.16280.soe.rass.gif")

 , Record (Just 901116) (ObsId 16194) (Just 0) "30 Doradus" "2014:132:20:19:02.746"  31.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.84  25.34 (Just "http://asc.harvard.edu/targets/901116/901116.16194.soe.dss.gif") (Just "http://asc.harvard.edu/targets/901116/901116.16194.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/901116/901116.16194.soe.rass.gif")

 , Record Nothing (SpecialObs "T_E39") Nothing "CAL-ER (52778)" "2014:133:05:50:04.996"   0.1 Nothing Nothing 219.8433 (-42.5680) 337.85 154.31  63.59 Nothing Nothing Nothing

 , Record Nothing (SpecialObs "GG_39") Nothing "CAL-ER (52777)" "2014:133:08:03:00.000"   0.1 Nothing Nothing 236.0147   6.4846 166.37 154.45 177.31 Nothing Nothing Nothing

 , Record Nothing (SpecialObs "G1_39") Nothing "CAL-ER (52776)" "2014:133:12:52:26.688"   0.3 Nothing Nothing 19.0000  60.0000  41.91  47.19 107.40    Nothing Nothing Nothing

 , Record Nothing (SpecialObs "T_X40") Nothing "CAL-ER (52775)" "2014:133:15:16:38.193"   0.1 Nothing Nothing 224.0000   6.0000 193.94 154.84 111.53 Nothing Nothing Nothing  

 , Record (Just 502237) (ObsId 16004) (Just 0) "Kepler's Supernova R" "2014:133:18:20:16.193" 104.0 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 149.48 113.86 (Just "http://asc.harvard.edu/targets/502237/502237.16004.soe.dss.gif") (Just "http://asc.harvard.edu/targets/502237/502237.16004.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/502237/502237.16004.soe.rass.gif")

 , Record (Just 702754) (ObsId 14517) (Just 1) "Q2237+0305" "2014:134:23:46:47.815"  30.0 (Just ACISS) (Just NONE) 340.1442   3.3410 108.61  71.29  79.62 (Just "http://asc.harvard.edu/targets/702754/702754.14517.soe.dss.gif") (Just "http://asc.harvard.edu/targets/702754/702754.14517.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/702754/702754.14517.soe.rass.gif")

 , Record (Just 901116) (ObsId 16615) (Just 0) "30 Doradus" "2014:135:08:44:31.003"  45.8 (Just ACISI) (Just NONE)  84.6976 (-69.0826) 212.02  90.97  99.99 (Just "http://asc.harvard.edu/targets/901116/901116.16615.soe.dss.gif") (Just "http://asc.harvard.edu/targets/901116/901116.16615.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/901116/901116.16615.soe.rass.gif")

 , Record Nothing (SpecialObs "T_E40") Nothing "CAL-ER (52773)" "2014:135:22:09:14.231"   0.1 Nothing Nothing 220.1000 (-42.1000) 331.99 154.60  64.54   Nothing Nothing Nothing

 , Record Nothing (SpecialObs "GG_40") Nothing "CAL-ER (52772)" "2014:136:00:22:00.000"   0.3 Nothing Nothing 67.0000 (-26.0000) 198.83  47.13 107.51   Nothing Nothing Nothing

 , Record Nothing (SpecialObs "G1_40") Nothing "CAL-ER (52771)" "2014:136:01:43:19.088"   0.8 Nothing Nothing 105.0000  11.0000 254.38  51.09  71.19   Nothing Nothing Nothing

 , Record Nothing (SpecialObs "G2_40") Nothing "CAL-ER (52770)" "2014:136:04:29:42.928"   0.5 Nothing Nothing  62.0271 (-29.2154) 191.69  49.08  78.13 Nothing Nothing Nothing

 , Record Nothing (SpecialObs "T_X41") Nothing "CAL-ER (52769)" "2014:136:06:59:34.670"   0.1 Nothing Nothing 0.7666   8.5993 107.55  51.42  96.58   Nothing Nothing Nothing

 , Record (Just 502237) (ObsId 16614) (Just 0) "Kepler's Supernova R" "2014:136:09:03:12.659"  36.8 (Just ACISS) (Just NONE) 262.6846 (-21.5042)  92.50 152.00 100.61 (Just "http://asc.harvard.edu/targets/502237/502237.16614.soe.dss.gif") (Just "http://asc.harvard.edu/targets/502237/502237.16614.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/502237/502237.16614.soe.rass.gif")

 , Record (Just 601116) (ObsId 16028) (Just 1) "NGC 300" "2014:136:19:53:38.301"  65.0 (Just ACISI) (Just NONE)  13.7470 (-37.6893) 130.01  67.94  97.17 (Just "http://asc.harvard.edu/targets/601116/601116.16028.soe.dss.gif") (Just "http://asc.harvard.edu/targets/601116/601116.16028.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/601116/601116.16028.soe.rass.gif")

 , Record (Just 801381) (ObsId 16525) (Just 0) "Abell S0295" "2014:137:14:24:15.301"  45.0 (Just ACISI) (Just NONE)  41.3955 (-53.0284) 167.50  73.16  29.99 (Just "http://asc.harvard.edu/targets/801381/801381.16525.soe.dss.gif") (Just "http://asc.harvard.edu/targets/801381/801381.16525.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/801381/801381.16525.soe.rass.gif")

  -- TODO: I think the SpecialObs value here is an error in the input 
 , Record Nothing (SpecialObs "I5190") Nothing "CAL-ER (52768)" "2014:138:03:36:44.209"   0.6 Nothing Nothing 190.2715  (-5.1604) 247.67 134.54 121.42   Nothing Nothing Nothing

 , Record (Just 801315) (ObsId 15190) (Just 0) "A1612" "2014:138:04:14:01.209"  29.1 (Just ACISI) (Just NONE) 191.9626  (-2.7668) 229.62 135.10  18.40 (Just "http://asc.harvard.edu/targets/801315/801315.15190.soe.dss.gif") (Just "http://asc.harvard.edu/targets/801315/801315.15190.soe.pspc.gif") (Just "http://asc.harvard.edu/targets/801315/801315.15190.soe.rass.gif")

 , Record Nothing (SpecialObs "T_E41") Nothing "CAL-ER (52766)" "2014:138:13:12:14.645"   0.1 Nothing Nothing 219.7761 (-42.5475) 334.00 153.64 101.50   Nothing Nothing Nothing
 
 ]
