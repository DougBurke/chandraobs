{-# LANGUAGE OverloadedStrings #-}

--
-- parser for the Chandra Short-Term schecule
--
-- there is some historical cruft here...
--

{-

example

601024    14528 0            NGC1399 2014:187:22:11:48.855  31.7 ACIS-S NONE  54.6182 -35.4286 132.06  75.60 155.00 dss pspc rass
703022    16632 0            Mkn 766 2014:188:07:50:05.556  27.0 ACIS-S HETG 184.6056  29.8371 252.00  69.38 155.00 dss pspc rass
801396    16633 0  Ophiuchus Cluster 2014:188:15:55:38.177  10.5 ACIS-I NONE 258.1023 -23.3778 266.99 153.83  90.20 dss pspc rass
 ----     T_E60       CAL-ER (52652) 2014:188:19:33:37.156   0.1   --    --  231.0000  17.0000 235.80 112.68  56.15   
 ----     GG_60       CAL-ER (52651) 2014:188:22:28:05.631   0.7   --    --  345.3150 -13.9093 108.01 124.12 151.00   
 ----     G1_60       CAL-ER (52650) 2014:189:00:50:06.836   0.1   --    --  315.0000 -36.0000  68.73 152.21  43.35   
 ----     T_X61       CAL-ER (52649) 2014:189:04:34:18.157   0.1   --    --   61.1048 -14.0627 128.41  58.03  94.29   
401583    15786 2           SXP 1062 2014:189:07:34:13.156  29.0 ACIS-I NONE  21.9880 -73.5610 101.15 110.18  63.65 dss pspc rass
703022    16311 0            Mkn 766 2014:189:16:23:07.892 135.0 ACIS-S HETG 184.6056  29.8371 252.00  68.26 135.36 dss pspc rass
401562    16636 0         4U 1626-67 2014:191:06:33:04.040  10.5  HRC-S LETG 248.0058 -67.4569 309.94 128.06 109.96 dss pspc rass
 ----     T_E61       CAL-ER (52648) 2014:191:10:00:21.414   0.1   --    --  134.5944 -20.8476 211.19  49.61  79.26   
 ----     GG_61       CAL-ER (52647) 2014:191:13:52:56.313   0.1   --    --   52.0000  41.0000  87.03  51.46 156.95   
 ----     G1_61       CAL-ER (52646) 2014:191:17:13:28.720   0.2   --    --   76.3982 -17.3321 139.32  51.23  83.33   
 ----     G2_61       CAL-ER (52645) 2014:191:18:32:48.581   0.2   --    --  110.0000  70.0000 359.69  47.81 137.89   
 ----     T_X62       CAL-ER (52644) 2014:191:20:43:02.414   0.1   --    --  128.0000 -23.0000 202.66  48.50 157.34   
401562    15765 0         4U 1626-67 2014:191:23:29:41.414  66.0  HRC-S LETG 248.0058 -67.4569 309.94 127.75  79.38 dss pspc rass
801396    16634 0  Ophiuchus Cluster 2014:192:18:19:58.048  22.6 ACIS-I NONE 258.1023 -23.3778 266.99 149.90  66.50 dss pspc rass
702749    14512 1       RXJ1131-1231 2014:193:01:10:05.807  11.0 ACIS-S NONE 172.9657 -12.5078 240.66  69.81  80.83 dss pspc rass
703008    16530 0           3C 390.3 2014:193:04:53:12.609  46.0 ACIS-S HETG 280.6465  79.7829 179.89  78.09 109.26 dss pspc rass
702966 P1 16073 0      SDSS2210+1137 2014:193:18:13:20.703   4.0 ACIS-S NONE 185.2671  11.6556 250.66  70.52  80.86 dss pspc rass
801396    16635 0  Ophiuchus Cluster 2014:193:19:53:12.172  18.6 ACIS-I NONE 258.1023 -23.3778 266.99 148.90  79.61 dss pspc rass
 ----     T_E62       CAL-ER (52642) 2014:194:01:39:18.609   0.1   --    --  270.0000 -42.0000 310.59 152.67  42.46   
 ----     GG_62       CAL-ER (52641) 2014:194:04:40:18.629   0.8   --    --  285.0000   2.0000 196.06 155.12 125.28   
 ----     ECT62       CAL-ER (52640) 2014:194:08:27:02.831   0.4   --    --  268.0000  40.0000 204.81 114.11  41.07   
 ----     G2_62       CAL-ER (52639) 2014:194:10:14:02.932   0.4   --    --  207.8079 -48.6439 258.94 109.65 115.45   
 ----     T_X63       CAL-ER (52638) 2014:194:11:51:04.382   0.1   --    --  119.0000 -38.0000 196.00  60.12  61.96   
801396    16143 0  Ophiuchus Cluster 2014:194:14:45:29.404  15.0 ACIS-I NONE 258.1023 -23.3778 266.99 148.14 111.91 dss pspc rass
401562    16637 0         4U 1626-67 2014:194:19:25:46.038  45.5  HRC-S LETG 248.0058 -67.4569 309.94 126.49  66.50 dss pspc rass

Note that mid 2017 CAL-ER runs started being labelled using a different
scheme; the number and id have been swapped:

 ----     P5402       CAL-ER (50082) 2017:134:17:33:39.022   0.3   --    --  246.0000  19.0000 158.47 139.67  15.88   

compared to

 ----     50081       CAL-ER (P5403) 2017:134:20:02:30.587   1.0   --    --  240.0000   4.0000 160.21 155.78  16.11   
703131    20079 0 Cygnus A - Nucleus 2017:141:17:30:58.135  24.0 ACIS-I NONE 299.8909  40.7272 125.98  96.57  84.38 dss pspc rass

In June 2022 we now have the following, can we use the same scheme, just handlig what is now labelled the NB
column with both "old" and "new" forms?

Seq #  NB  ObsID Constr.          Target                Start        Time   SI   Grat    RA       Dec    Roll   Pitch   Slew   Overlays
------ --- ----- ------- -------------------- --------------------- ----- ------ ---- -------- -------- ------ ------ ------ -------------
704446     25402    1             ASASSN-14ko 2022:171:11:06:14.675  34.0 ACIS-S NONE  81.3615 -45.9964 175.03  69.71 106.45 dss pspc rass
901512     23948    0                   GDH13 2022:171:21:11:30.290   5.3 ACIS-I NONE 266.6376 -31.2877 343.66 171.79 103.92 dss pspc rass
 ----      45419               CAL-ER (PER01) 2022:173:05:04:19.472   3.0   --    --  159.0000  48.0000 268.95  58.62  57.82   
503371 DDT 26441    0              GRB220611A 2022:174:23:57:25.983  15.0 ACIS-S NONE  66.5449 -37.2560 164.02  65.33 123.3
-}

module Parser (parseSTS
              , testParser
              , parseReadable
              , parseRAStr
              , parseDecStr
              , parseTime
              , handleTime) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Char (digitToInt, isDigit, isSpace)
import Data.Functor (void)
import Data.List (intercalate)

import Formatting (int, sformat)

import Text.Parsec

import Types (ScheduleItem(..)
             , TimeKS
             , unsafeToTimeKS
             , ChandraTime
             , toCTime, endCTime
             , RA
             , toRA
             , Dec
             , toDec
             , Instrument, Grating
             , unsafeToObsIdVal
             )

type Parser = Parsec String ()

showInt :: Int -> T.Text
showInt = sformat int

testParser :: String -> IO ()
testParser =
  let lbl as = T.putStrLn ("found " <> showInt (length as) <> " records")
  in either print lbl . parseSTS

commentLine :: Parser ()
commentLine = comments >> void (char eol)

-- A comment character and then up to the end of the line
comments :: Parser ()
comments = try (char '#') >> skipMany (satisfy (/= eol))

eol :: Char
eol = '\n'

-- hacky way to allow comment lines; rather fragile
-- as coded - e.g. can not end with a comment
parseSTS :: String -> Either ParseError [ScheduleItem]
parseSTS = parse p "<STS parsing>"
  where
    p = do
      spaces
      sts <- many stsLine
      skipMany comments
      eof
      pure sts

lexeme :: Parser p -> Parser p
lexeme p = p >>= \a -> spaces >> pure a

toInt :: [Int] -> Int
toInt = foldl (\c i -> c * 10 + i) 0

strToInt :: String -> Int
strToInt = toInt . map digitToInt

-- for now I do not check for overflow
parseInt :: Parser Int
parseInt = strToInt <$> lexeme (many1 digit)

-- parse a single integer value
parseInt1 :: Parser Int
parseInt1 = digitToInt `fmap` lexeme digit

colon :: Parser ()
colon = void (char ':')

-- just ensure we have yyyy:ddd:hh:mm:ss.sss;
-- it seems a bit wasteful to split up the string and
-- then reconstitute it
parseTime :: Parser String
parseTime = do
  year <- count 4 digit
  colon
  dnum <- count 3 digit
  colon
  hour <- count 2 digit
  colon
  mins <- count 2 digit
  colon
  ss1 <- count 2 digit
  void (char '.')
  ss2 <- count 3 digit
  spaces
  pure (intercalate ":" [year, dnum, hour, mins, ss1 ++ "." ++ ss2])

parseReadable :: Read a => Parser a
parseReadable = do
  v <- lexeme $ many1 $ satisfy (not . isSpace)
  case reads v of
    [(t,"")] -> pure t
    _ -> fail ("Unable to parse as readable: " ++ v)

parseDouble :: Parser Double
parseDouble = parseReadable

-- Convert the old, HTML format RA and Dec, which uses sexagessimal
-- notation like "6: 9:56.09" and "7: 0:14.04".
--
parseColonSep :: Parser Double
parseColonSep = do
  a <- parseInt
  colon
  spaces
  b <- parseInt
  colon
  spaces
  c <- parseDouble
  let r = fromIntegral a + (fromIntegral b + (c / 60.0)) / 60.0
  pure r
  
parseRAStr :: Parser RA
parseRAStr = do
  r <- parseColonSep
  pure (toRA (15 * r))

parseDecStr :: Parser Dec
parseDecStr = do
  sign <- option 1 (char '-' >> pure (-1))
  spaces -- yes, there can be spaces after the sign!
  r <- parseColonSep
  pure (toDec (sign * r))

  
-- parsing the title really requires parsing the time and then picking
-- everything up until that point. The value of the title string is
-- not needed for science obs, but is for non-science obs. However,
-- this is only called for science obs.
--
-- Since speed is currently not a huge issue, just go ahead and
-- check that we actually have a time field
--
parseTitle :: Parser String
parseTitle = manyTill anyChar (try (lookAhead parseTime))

parseInst :: Parser Instrument
parseInst = parseReadable

parseGrat :: Parser Grating
parseGrat = parseReadable

handleTime :: String -> Double -> (TimeKS, ChandraTime, ChandraTime)
handleTime start texp =
  let tks = unsafeToTimeKS texp
      t1 = toCTime start
      t2 = endCTime t1 tks
  in (tks, t1, t2)


parsePosition :: Parser (RA, Dec, Double)
parsePosition = do
  ra <- parseReadable -- ra
  dec <- parseReadable -- dec
  roll <- parseDouble -- roll
  void parseDouble -- pitch
  void parseDouble -- slew
  pure (ra, dec, roll)


parsePvalue :: Parser String
parsePvalue = char 'P' >> digit >>= \c -> pure [c]

parseObsType :: Parser String
parseObsType = lexeme (parsePvalue <|>
                       string "DDT" <|>
                       string "TOO" <|>
                       string "CCT")


obsLine :: Parser ScheduleItem
obsLine = do
  void parseInt -- seqNum

  optional parseObsType

  obsid <- parseInt  
  void parseInt1 -- n
  void parseTitle -- title
  start <- parseTime
  texp <- parseDouble
  void parseInst -- inst
  void parseGrat -- grat
  (ra, dec, roll) <- parsePosition
  lexeme (void (string "dss pspc rass"))

  let (tks, t1, _) = handleTime start texp

      si = ScheduleItem {
        siObsId = unsafeToObsIdVal obsid
        , siScienceObs = True
        , siStart = t1
        -- , siEnd = t2
        , siDuration = tks
        , siRA = ra
        , siDec = dec
        , siRoll = roll
        }

  pure si

sep2, sep4 :: Parser ()
sep2 = void (lexeme (string "--"))
sep4 = void (lexeme (string "----"))

-- Calibration observations used to be
--  ----     P5402       CAL-ER (50082) 2017:134:17:33:39.022   0.3   --    --  246.0000  19.0000 158.47 139.67  15.88   
-- and appear to have switched over, mid 2017, to
--  ----     50081       CAL-ER (P5403) 2017:134:20:02:30.587   1.0   --    --  240.0000   4.0000 160.21 155.78  16.11
--
calLine :: Parser ScheduleItem
calLine = do
  sep4
  nameOrObsId <- lexeme (count 5 anyChar)
  void (string "CAL-ER (")
  -- obsid <- parseInt
  obsidOrName <- count 5 anyChar
  lexeme (void (string ")"))  -- err, why not void (lexeme (string))
  start <- parseTime
  texp <- parseDouble
  sep2
  sep2
  (ra, dec, roll) <- parsePosition
  --let title = "CAL-ER (" ++ show obsid ++ ")"
  --pure $ STS Nothing (SpecialObs n) Nothing title start texp Nothing Nothing ra dec roll pitch slew

  let (tks, t1, _) = handleTime start texp

      -- Not sure of the rules for obsidOrName; at the moment it
      -- appears that starting with a non-numeric character indicates
      -- it's the name. Although not encoded in the type, the strings
      -- should not be empty (due to both being created via
      -- 'take 5' above).
      --
      obsid =
        if isDigit (head obsidOrName)
        then strToInt obsidOrName
        else strToInt nameOrObsId
  
      si = ScheduleItem {
        siObsId = unsafeToObsIdVal obsid
        , siScienceObs = False
        , siStart = t1
        -- , siEnd = t2
        , siDuration = tks
        , siRA = ra
        , siDec = dec
        , siRoll = roll
        }

  pure si
  
stsLine :: Parser ScheduleItem
stsLine = do
  spaces
  optional (many (commentLine >> spaces))
  calLine <|> obsLine

