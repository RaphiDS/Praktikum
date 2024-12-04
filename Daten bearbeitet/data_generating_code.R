# Skript Raphi
#loading all Datasets and packages
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/NSDUH_2015.RData")
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/NSDUH_2016.RData")
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/NSDUH_2017.RData")
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/NSDUH_2018.RData")
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/NSDUH_2019.RData")
library(tidyverse)

#filtering the relevant columns and creating reduced dataset
filterdata2015 <- PUF2015_021518 %>%
  select(c(1:ALCBNG30D, cocever:CC30EST, herever:HR30EST, ircigrc:II2ALCRC, ircocrc:II2COCRC, irherrc:II2HERRC,
           iralcfy:II2ALCFY, ircocfy:II2COCFY, irherfy:II2HERFY, ircigfm:IIALCBNG30D, ircocfm:II2COCFM,
           irherfm:II2HERFM, ircigage:iialcyfu, ircocage:iicocyfu, irherage:iiheryfu, cigflag:alcmon,
           cocflag:cocmon, herflag:hermon, opinmyr:illmon, illemflag:illalcflg, cigafu:ALCAFU3, alcydays,
           cocydays, cigmdays:alcmdays, cocmdays, CIG1PACK:FUALC21, FUCOC18:FUCOC21, FUHER18:FUHER21,
           cocneedl:hrndlrec, anyndlrec:nedcoc, HERSMOK2:HERSNIF2, rskcigpkd, rskhertry:rskbngwk, difgetcoc,
           difgether, appdrgmon:rkfqdblt, grskcigpkd, grskcocmon:grskherwk, grskbngdly:grskbngwk, difobtcoc,
           difobther, APPDRGMON2, blntever:alcfmctd, coclottm:herfmctd, ircgirtb:iicglmr, ndssansp:depndalc,
           depndcoc:depndher, depndpyill:abusealc, abusecoc:abuseher, abusepyill:abodalc, abodcoc:abodher,
           udpyhrpnr:udpyilaal, booked:prob, cigaglst:cocmlu, heraglst:hermlu, cigyrbfr:cocyrbfr, txevrrcvd:ndmortalc,
           ndmortcoc:ndmorther, ndtxyralc, ndtxyrcoc:ndtxyrher, ndtxeffrt:txltyalco, txltycocn:txltyhern,
           txltyothr:TXLTYALCO2, TXLTYCOCN2:TXLTYHERN2, txltyill:verep)) %>%
  mutate(year = 2015)

filterdata2016 <- PUF2016_022818 %>%
  select(c(1:ALCBNG30D, cocever:CC30EST, herever:HR30EST, ircigrc:II2ALCRC, ircocrc:II2COCRC, irherrc:II2HERRC,
           iralcfy:II2ALCFY, ircocfy:II2COCFY, irherfy:II2HERFY, ircigfm:IIALCBNG30D, ircocfm:II2COCFM,
           irherfm:II2HERFM, ircigage:iialcyfu, ircocage:iicocyfu, irherage:iiheryfu, cigflag:alcmon,
           cocflag:cocmon, herflag:hermon, opinmyr:illmon, illemflag:illalcflg, cigafu:ALCAFU3, alcydays,
           cocydays, cigmdays:alcmdays, cocmdays, CIG1PACK:FUALC21, FUCOC18:FUCOC21, FUHER18:FUHER21,
           cocneedl:hrndlrec, anyndlrec:nedcoc, HERSMOK2:HERSNIF2, rskcigpkd, rskhertry:rskbngwk, difgetcoc,
           difgether, appdrgmon:rkfqdblt, grskcigpkd, grskcocmon:grskherwk, grskbngdly:grskbngwk, difobtcoc,
           difobther, APPDRGMON2, blntever:alcfmctd, coclottm:herfmctd, ircgirtb:iicglmr, ndssansp:depndalc,
           depndcoc:depndher, depndpyill:abusealc, abusecoc:abuseher, abusepyill:abodalc, abodcoc:abodher,
           udpyhrpnr:udpyilaal, booked:prob, cigaglst:cocmlu, heraglst:hermlu, cigyrbfr:cocyrbfr, txevrrcvd:ndmortalc,
           ndmortcoc:ndmorther, ndtxyralc, ndtxyrcoc:ndtxyrher, ndtxeffrt:txltyalco, txltycocn:txltyhern,
           txltyothr:TXLTYALCO2, TXLTYCOCN2:TXLTYHERN2, txltyill:verep)) %>%
  mutate(year = 2016)

filterdata2017 <- PUF2017_100918 %>%
  select(c(1:ALCBNG30D, cocever:CC30EST, herever:HR30EST, ircigrc:II2ALCRC, ircocrc:II2COCRC, irherrc:II2HERRC,
           iralcfy:II2ALCFY, ircocfy:II2COCFY, irherfy:II2HERFY, ircigfm:IIALCBNG30D, ircocfm:II2COCFM,
           irherfm:II2HERFM, ircigage:iialcyfu, ircocage:iicocyfu, irherage:iiheryfu, cigflag:alcmon,
           cocflag:cocmon, herflag:hermon, opinmyr:illmon, illemflag:illalcflg, cigafu:ALCAFU3, alcydays,
           cocydays, cigmdays:alcmdays, cocmdays, CIG1PACK:FUALC21, FUCOC18:FUCOC21, FUHER18:FUHER21,
           cocneedl:hrndlrec, anyndlrec:nedcoc, HERSMOK2:HERSNIF2, rskcigpkd, rskhertry:rskbngwk, difgetcoc,
           difgether, appdrgmon:rkfqdblt, grskcigpkd, grskcocmon:grskherwk, grskbngdly:grskbngwk, difobtcoc,
           difobther, APPDRGMON2, blntever:alcfmctd, coclottm:herfmctd, ircgirtb:iicglmr, ndssansp:depndalc,
           depndcoc:depndher, depndpyill:abusealc, abusecoc:abuseher, abusepyill:abodalc, abodcoc:abodher,
           udpyhrpnr:udpyilaal, booked:prob, cigaglst:cocmlu, heraglst:hermlu, cigyrbfr:cocyrbfr, txevrrcvd:ndmortalc,
           ndmortcoc:ndmorther, ndtxyralc, ndtxyrcoc:ndtxyrher, ndtxeffrt:txltyalco, txltycocn:txltyhern,
           txltyothr:TXLTYALCO2, TXLTYCOCN2:TXLTYHERN2, txltyill:verep)) %>%
  mutate(year = 2017)

filterdata2018 <- PUF2018_100819 %>%
  mutate(cigafu = NA, ALCAFU3 = NA, FUALC21 = NA, FUCOC18 = NA, FUCOC21 = NA, FUHER18 = NA, FUHER21 = NA) %>%
  select(c(1:ALCBNG30D, cocever:CC30EST, herever:HR30EST, ircigrc:II2ALCRC, ircocrc:II2COCRC, irherrc:II2HERRC,
           iralcfy:II2ALCFY, ircocfy:II2COCFY, irherfy:II2HERFY, ircigfm:IIALCBNG30D, ircocfm:II2COCFM,
           irherfm:II2HERFM, ircigage:iialcyfu, ircocage:iicocyfu, irherage:iiheryfu, cigflag:alcmon,
           cocflag:cocmon, herflag:hermon, opinmyr:illmon, illemflag:illalcflg, cigafu:ALCAFU3, alcydays,
           cocydays, cigmdays:alcmdays, cocmdays, CIG1PACK:FUALC21, FUCOC18:FUCOC21, FUHER18:FUHER21,
           cocneedl:hrndlrec, anyndlrec:nedcoc, HERSMOK2:HERSNIF2, rskcigpkd, rskhertry:rskbngwk, difgetcoc,
           difgether, appdrgmon:rkfqdblt, grskcigpkd, grskcocmon:grskherwk, grskbngdly:grskbngwk, difobtcoc,
           difobther, APPDRGMON2, blntever:alcfmctd, coclottm:herfmctd, ircgirtb:iicglmr, ndssansp:depndalc,
           depndcoc:depndher, depndpyill:abusealc, abusecoc:abuseher, abusepyill:abodalc, abodcoc:abodher,
           udpyhrpnr:udpyilaal, booked:prob, cigaglst:cocmlu, heraglst:hermlu, cigyrbfr:cocyrbfr, txevrrcvd:ndmortalc,
           ndmortcoc:ndmorther, ndtxyralc, ndtxyrcoc:ndtxyrher, ndtxeffrt:txltyalco, txltycocn:txltyhern,
           txltyothr:TXLTYALCO2, TXLTYCOCN2:TXLTYHERN2, txltyill:verep)) %>%
  mutate(year = 2018)

filterdata2019 <- PUF2019_100920 %>%
  mutate(cigafu = NA, ALCAFU3 = NA, FUALC21 = NA, FUCOC18 = NA, FUCOC21 = NA, FUHER18 = NA, FUHER21 = NA) %>%
  select(c(1:ALCBNG30D, cocever:CC30EST, herever:HR30EST, ircigrc:II2ALCRC, ircocrc:II2COCRC, irherrc:II2HERRC,
           iralcfy:II2ALCFY, ircocfy:II2COCFY, irherfy:II2HERFY, ircigfm:IIALCBNG30D, ircocfm:II2COCFM,
           irherfm:II2HERFM, ircigage:iialcyfu, ircocage:iicocyfu, irherage:iiheryfu, cigflag:alcmon,
           cocflag:cocmon, herflag:hermon, opinmyr:illmon, illemflag:illalcflg, cigafu:ALCAFU3, alcydays,
           cocydays, cigmdays:alcmdays, cocmdays, CIG1PACK:FUALC21, FUCOC18:FUCOC21, FUHER18:FUHER21,
           cocneedl:hrndlrec, anyndlrec:nedcoc, HERSMOK2:HERSNIF2, rskcigpkd, rskhertry:rskbngwk, difgetcoc,
           difgether, appdrgmon:rkfqdblt, grskcigpkd, grskcocmon:grskherwk, grskbngdly:grskbngwk, difobtcoc,
           difobther, APPDRGMON2, blntever:alcfmctd, coclottm:herfmctd, ircgirtb:iicglmr, ndssansp:depndalc,
           depndcoc:depndher, depndpyill:abusealc, abusecoc:abuseher, abusepyill:abodalc, abodcoc:abodher,
           udpyhrpnr:udpyilaal, booked:prob, cigaglst:cocmlu, heraglst:hermlu, cigyrbfr:cocyrbfr, txevrrcvd:ndmortalc,
           ndmortcoc:ndmorther, ndtxyralc, ndtxyrcoc:ndtxyrher, ndtxeffrt:txltyalco, txltycocn:txltyhern,
           txltyothr:TXLTYALCO2, TXLTYCOCN2:TXLTYHERN2, txltyill:verep)) %>%
  mutate(year = 2019)

allfilterdata <- rbind(filterdata2015[Reduce(intersect,list(colnames(filterdata2015), colnames(filterdata2016),
                      colnames(filterdata2017),colnames(filterdata2018), colnames(filterdata2019)))],
                      filterdata2016[Reduce(intersect,list(colnames(filterdata2015), colnames(filterdata2016),
                      colnames(filterdata2017),colnames(filterdata2018), colnames(filterdata2019)))],
                      filterdata2017[Reduce(intersect,list(colnames(filterdata2015), colnames(filterdata2016),
                      colnames(filterdata2017),colnames(filterdata2018), colnames(filterdata2019)))],
                      filterdata2018[Reduce(intersect,list(colnames(filterdata2015), colnames(filterdata2016),
                      colnames(filterdata2017),colnames(filterdata2018), colnames(filterdata2019)))],
                      filterdata2019[Reduce(intersect,list(colnames(filterdata2015), colnames(filterdata2016),
                      colnames(filterdata2017),colnames(filterdata2018), colnames(filterdata2019)))])

save(allfilterdata, file = "combi_redu_data.Rdata")