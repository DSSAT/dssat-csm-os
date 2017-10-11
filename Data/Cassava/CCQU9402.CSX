*EXP.DETAILS: CCQU9402CS WATER STRESS AND FERTILIZATION

*GENERAL
@PEOPLE
SARA MEJIA DE TAFUR 
@ADDRESS
CIAT,A.A.6713,CALI,COLOMBIA
@SITE
SANTANDER DE QUILICHAO,COLOMBIA  3.10;-76.51;990;CW
@ PAREA  PRNO  PLEN  PLDR  PLSP  PLAY HAREA  HRNO  HLEN  HARM.........
    -99   -99   -99   -99   -99   -99   -99   -99   -99   -99

*TREATMENTS                        -------------FACTOR LEVELS------------
@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
 1 1 1 0 MCol-1684 stress fert      1  1  0  1  1  0  1  0  0  0  1  1  1
 2 1 1 0 MCol-1684 stress no fert   1  1  0  1  1  0  0  0  0  0  1  1  1
 3 1 1 0 CM 507-37 stress fert      2  1  0  1  1  0  1  0  0  0  1  1  1
 4 1 1 0 CM 507-37 stress no fert   2  1  0  1  1  0  0  0  0  0  1  1  1
 5 1 1 0 MCol-1684 no stress fert   1  1  0  1  1  0  1  0  0  0  0  1  2
 6 1 1 0 MCol-1684 no stress no f   1  1  0  1  1  0  0  0  0  0  0  1  2
 7 1 1 0 CM 507-37 no stress fert   2  1  0  1  1  0  1  0  0  0  0  1  2
 8 1 1 0 CM 507-37 no stress no f   2  1  0  1  1  0  0  0  0  0  0  1  2
*CULTIVARS
@C CR INGENO CNAME
 1 CS UC0006 MCol-1684
 2 CS UC0014 CM 507-37

*FIELDS
@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
 1 CCQU0001 CCQU       -99   -99   -99   -99   -99   -99 -99    -99  CCQU000033 -99
@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR
 1             -99             -99       -99               -99   -99   -99   -99   -99   -99

*INITIAL CONDITIONS
@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME
 1    CS 93089   -99   -99     1     1   -99   -99   -99   -99   -99   -99 -99
@C  ICBL  SH2O  SNH4  SNO3
 1     5   .47    .1    .8
 1    15  .473    .1    .8
 1    28  .491    .1    .8
 1    44   .51    .1    .8
 1    65  .527    .1    .8
 1    96  .536    .1    .8
 1   122  .549    .1    .8
 1   150  .553    .1    .8
 1   178  .541    .1    .8
 1   196  .488    .1    .8
 1   230  .488    .1    .8
 1   260  .488    .1    .8

*PLANTING DETAILS
@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME
 1 93089   -99     1     1     V     R   100   -99    20   450   -99   -99   -99    25                        -99

*FERTILIZERS (INORGANIC)
@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
 1 93089 FE020 AP001     5     0     0     0   500   -99   -99 -99
 1 93120 FE005 AP001     5    50     0     0     0   -99   -99 -99
 1 93120 FE021 AP001     5     0    44     0     0   -99   -99 -99
 1 93120 FE016 AP001     5     0     0    83   300   -99   -99 -99

*ENVIRONMENT MODIFICATIONS
@E ODATE EDAY  ERAD  EMAX  EMIN  ERAIN ECO2  EDEW  EWIND ENVNAME  
 1 93209 A 0.0 A 0.0 A   0 A   0 M 0.0 A   0 A   0 A   0 water stress
 1 94087 A 0.0 A 0.0 A   0 A   0 M 1.0 A   0 A   0 A   0 water stress


*HARVEST DETAILS
@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME
 1 94087 GS000   -99   -99   -99   -99 Cassava

*SIMULATION CONTROLS
@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 1 GE              1     1     S 93089  2150 DEFAULT SIMULATION CONTR
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 1 OP              Y     Y     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 1 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 1 MA              R     N     R     N     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 1 OU              N     Y     Y     1     Y     Y     Y     Y     Y     N     Y     N     Y

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 1 PL            -99   -99    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 1 IR             30    50   100 GS000 IR001    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 1 NI             30    50    25 FE001 GS000
@N RESIDUES    RIPCN RTIME RIDEP
 1 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 1 HA              0   -99   100     0
 
@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 2 GE              1     1     S 93089  2150 DEFAULT SIMULATION CONTR
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 2 OP              Y     Y     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 2 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 2 MA              R     A     R     N     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 2 OU              N     Y     Y     1     Y     Y     Y     Y     Y     N     Y     N     Y

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 2 PL            -99   -99    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 2 IR             50    10    50 GS000 IR001    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 2 NI             30    50    25 FE001 GS000
@N RESIDUES    RIPCN RTIME RIDEP
 2 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 2 HA              0   -99   100     0



