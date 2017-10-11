*EXP.DETAILS: CCPA7601CS 1976 Branching habits 

*GENERAL
@PEOPLE
TAN SWEE LIAN AND JAMES COCK
@ADDRESS
-99
@SITE
PALMIRA,COLOMBIA  3.52;-76.35;423;AW
@ PAREA  PRNO  PLEN  PLDR  PLSP  PLAY HAREA  HRNO  HLEN  HARM.........
    -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
@NOTES
Branching habit as a yield dterminant in cassava

*TREATMENTS                        -------------FACTOR LEVELS------------
@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
 1 1 0 0 MCol-113 4br               1  1  0  0  1  0  0  0  0  0  0  1  1
 2 1 0 0 MCol-113 3br               2  1  0  0  1  0  0  0  0  0  0  1  1
 3 1 0 0 MCol-113 2br               3  1  0  0  1  0  0  0  0  0  0  1  1
 4 1 0 0 MCol-113 1br               4  1  0  0  1  0  0  0  0  0  0  1  1
 5 1 0 0 MCol-113 1-4br             5  1  0  0  1  0  0  0  0  0  0  1  1

*CULTIVARS
@C CR INGENO CNAME
 1 CS UC0011 MCol-113
 2 CS UC0016 MCol-113 3br
 3 CS UC0017 MCol-113 2br
 4 CS UC0018 MCol-113 1br
 5 CS UC0019 MCol-113 1-4br
 
*FIELDS
@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
 1 CIAT0001 CCPA7602   -99   -99   -99   -99   -99   -99 -99    209  CCPA000030 -99
@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR
 1               0               0         0                 0     0     0     0   -99   -99

*PLANTING DETAILS
@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME
 1 76101   -99     1     1     I   -99   100    90    20   450   -99   -99   -99    20                        -99
*IRRIGATION AND WATER MANAGEMENT
@I  EFIR  IDEP  ITHR  IEPT  IOFF  IAME  IAMT IRNAME
 1     1    30    50   100   -99   -99    10 -99
@I IDATE  IROP IRVAL
 1 76208 IR001    20
 1 76215 IR001    20
 1 76222 IR001    20
 1 76229 IR001    20
 1 76236 IR001    20
 1 76243 IR001    20
 1 76250 IR001    20

 
*FERTILIZERS (INORGANIC)
@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
 1 76108 FE005 AP001    20   100     0     0     0     0   -99 -99
 1 76108 FE006 AP001    20     0   100     0     0     0   -99 -99
 1 76108 FE018 AP016    20     0     0   100     0     0   -99 -99 
 
*HARVEST DETAILS
@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME
 1 77050 GS000   -99   -99   -99   -99 

*SIMULATION CONTROLS
@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 1 GE              1     1     P 76050  2150 Cultivars  Lian
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 1 OP              N     N     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 1 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 1 MA              R     N     N     N     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 1 OU              N     Y     Y     1     Y     N     Y     Y     N     N     Y     N     N

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 1 PL          76050 76150    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 1 IR             30    50   100 IB001 IBI01    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 1 NI             30    50    25 IB001 IB001
@N RESIDUES    RIPCN RTIME RIDEP
 1 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 1 HA          77050 77150   100     0



