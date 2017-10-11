*EXP.DETAILS: CCQU9401CS PHOTOSYNTHESIS 1994

*GENERAL
@PEOPLE
M. EL-SHARKAWY - S.DE TAFUR
@ADDRESS
CIAT,A.A.6713,CALI,COLOMBIA
@SITE
SANTANDER DE QUILICHAO,COLOMBIA  3.10;-76.51;990;CW
@ PAREA  PRNO  PLEN  PLDR  PLSP  PLAY HAREA  HRNO  HLEN  HARM.........
    -99   -99   -99   -99   -99   -99   -99   -99   -99   -99

*TREATMENTS                        -------------FACTOR LEVELS------------
@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
 1 1 1 0 Planting 1994              1  1  0  1  1  0  1  0  0  0  0  1  1

*CULTIVARS
@C CR INGENO CNAME
 1 CS UC0002 MCol-22

*FIELDS
@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
 1 CCQU0001 CCQU       -99   -99   -99   -99   -99   -99 -99    -99  CCQU000033 -99
@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR
 1             -99             -99       -99               -99   -99   -99   -99   -99   -99

*INITIAL CONDITIONS
@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME
 1    CS 94277   -99   -99     1     1   -99   -99   -99   -99   -99   -99 -99
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
 1 94277   -99     1     1     V     R   100   -99    20   400   -99   -99   -99    20                        -99

*FERTILIZERS (INORGANIC)
@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
 1 94307 FE001 AP001    20    50     0     0     0     0   -99 -99
 1 94307 FE013 AP001    20     0    44     0     0     0   -99 -99
 1 94307 FE016 AP001    20     0     0    84     0     0   -99 -99
 
*HARVEST DETAILS
@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME
 1 95272     6     H     A   100   -99  
 
*SIMULATION CONTROLS
@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 1 GE              1     1     S 94277  2150 DEFAULT SIMULATION CONTR
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 1 OP              Y     Y     N     N     N     N     N     N     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 1 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 1 MA              R     N     R     N     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 1 OU              N     Y     Y     1     Y     Y     Y     Y     Y     N     Y     N     Y

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 1 PL          94277 94277    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 1 IR             30    50   100 GS000 IR001    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 1 NI             30    50    25 FE001 GS000
@N RESIDUES    RIPCN RTIME RIDEP
 1 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 1 HA              0 94277   100     0



