*EXP.DETAILS: HIMA8801CS 1988 3 TEMPERATURES HAWAII

*GENERAL
@PEOPLE
Manrique (1992)
@ADDRESS
IITA
@SITE
Hawaii, Mt. Haleakala, Maui, HI (20° N lat)
@ PAREA  PRNO  PLEN  PLDR  PLSP  PLAY HAREA  HRNO  HLEN  HARM.........
    -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
@NOTES
 ETH and IITA experiment

*TREATMENTS                        -------------FACTOR LEVELS------------
@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
 1 1 0 0 282 m                      1  1  0  0  1  0  0  0  0  0  0  1  1
 2 1 0 0 640 m                      1  2  0  0  1  0  0  0  0  0  0  1  1
 3 1 0 0 1097 m                     1  3  0  0  1  0  0  0  0  0  0  1  1

*CULTIVARS
@C CR INGENO CNAME
 1 CS UC0005 Ceiba

*FIELDS
@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
 1 MAUI0001 MAUI8801   -99   -99   -99   -99   -99   -99 -99    209  IBAR910035 282 m
 2 MAUI0002 MAUI8802   -99   -99   -99   -99   -99   -99 -99    209  IBAR910035 640 m
 3 MAUI0003 MAUI8803   -99   -99   -99   -99   -99   -99 -99    209  IBAR910035 1097 m
@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR
 1               0               0       282                 0     0     0     0   -99   -99
 2               0               0       640                 0     0     0     0   -99   -99
 3               0               0      1097                 0     0     0     0   -99   -99

*PLANTING DETAILS
@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME
 1 88041   -99     1     1     V     R   100    90    20   450   -99   -99   -99    25                        -99

*HARVEST DETAILS
@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME
 1 88290 GS000   -99   -99   -99   -99 

*SIMULATION CONTROLS
@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 1 GE              1     1     P 88020  2150 
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 1 OP              N     N     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 1 ME              G     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 1 MA              R     N     N     N     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT FMOPT
 1 OU              N     Y     Y     1     Y     N     Y     Y     N     N     Y     N     N      

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 1 PL          88135 88200    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 1 IR             30    50   100 IB001 IBI01    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 1 NI             30    50    25 IB001 IB001
@N RESIDUES    RIPCN RTIME RIDEP
 1 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 1 HA              0 88349   100     0



